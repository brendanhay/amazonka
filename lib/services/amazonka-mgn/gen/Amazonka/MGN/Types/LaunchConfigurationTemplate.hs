{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MGN.Types.LaunchConfigurationTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.LaunchConfigurationTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MGN.Types.PostLaunchActions
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newLaunchConfigurationTemplate' smart constructor.
data LaunchConfigurationTemplate = LaunchConfigurationTemplate'
  { -- | Copy Private IP during Launch Configuration.
    tags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Copy Private IP during Launch Configuration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Copy Private IP during Launch Configuration.
    postLaunchActions :: Prelude.Maybe PostLaunchActions,
    -- | Copy Private IP during Launch Configuration.
    launchConfigurationTemplateID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchConfigurationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'launchConfigurationTemplate_tags' - Copy Private IP during Launch Configuration.
--
-- 'arn', 'launchConfigurationTemplate_arn' - Copy Private IP during Launch Configuration.
--
-- 'postLaunchActions', 'launchConfigurationTemplate_postLaunchActions' - Copy Private IP during Launch Configuration.
--
-- 'launchConfigurationTemplateID', 'launchConfigurationTemplate_launchConfigurationTemplateID' - Copy Private IP during Launch Configuration.
newLaunchConfigurationTemplate ::
  -- | 'launchConfigurationTemplateID'
  Prelude.Text ->
  LaunchConfigurationTemplate
newLaunchConfigurationTemplate
  pLaunchConfigurationTemplateID_ =
    LaunchConfigurationTemplate'
      { tags =
          Prelude.Nothing,
        arn = Prelude.Nothing,
        postLaunchActions = Prelude.Nothing,
        launchConfigurationTemplateID =
          pLaunchConfigurationTemplateID_
      }

-- | Copy Private IP during Launch Configuration.
launchConfigurationTemplate_tags :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
launchConfigurationTemplate_tags = Lens.lens (\LaunchConfigurationTemplate' {tags} -> tags) (\s@LaunchConfigurationTemplate' {} a -> s {tags = a} :: LaunchConfigurationTemplate) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | Copy Private IP during Launch Configuration.
launchConfigurationTemplate_arn :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe Prelude.Text)
launchConfigurationTemplate_arn = Lens.lens (\LaunchConfigurationTemplate' {arn} -> arn) (\s@LaunchConfigurationTemplate' {} a -> s {arn = a} :: LaunchConfigurationTemplate)

-- | Copy Private IP during Launch Configuration.
launchConfigurationTemplate_postLaunchActions :: Lens.Lens' LaunchConfigurationTemplate (Prelude.Maybe PostLaunchActions)
launchConfigurationTemplate_postLaunchActions = Lens.lens (\LaunchConfigurationTemplate' {postLaunchActions} -> postLaunchActions) (\s@LaunchConfigurationTemplate' {} a -> s {postLaunchActions = a} :: LaunchConfigurationTemplate)

-- | Copy Private IP during Launch Configuration.
launchConfigurationTemplate_launchConfigurationTemplateID :: Lens.Lens' LaunchConfigurationTemplate Prelude.Text
launchConfigurationTemplate_launchConfigurationTemplateID = Lens.lens (\LaunchConfigurationTemplate' {launchConfigurationTemplateID} -> launchConfigurationTemplateID) (\s@LaunchConfigurationTemplate' {} a -> s {launchConfigurationTemplateID = a} :: LaunchConfigurationTemplate)

instance Core.FromJSON LaunchConfigurationTemplate where
  parseJSON =
    Core.withObject
      "LaunchConfigurationTemplate"
      ( \x ->
          LaunchConfigurationTemplate'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "postLaunchActions")
            Prelude.<*> (x Core..: "launchConfigurationTemplateID")
      )

instance Prelude.Hashable LaunchConfigurationTemplate where
  hashWithSalt _salt LaunchConfigurationTemplate' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` postLaunchActions
      `Prelude.hashWithSalt` launchConfigurationTemplateID

instance Prelude.NFData LaunchConfigurationTemplate where
  rnf LaunchConfigurationTemplate' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf postLaunchActions
      `Prelude.seq` Prelude.rnf launchConfigurationTemplateID
