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
-- Module      : Amazonka.Kendra.Types.ExperienceConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ExperienceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types.ContentSourceConfiguration
import Amazonka.Kendra.Types.UserIdentityConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for your Amazon Kendra
-- experience. This includes the data source IDs and\/or FAQ IDs, and user
-- or group information to grant access to your Amazon Kendra experience.
--
-- /See:/ 'newExperienceConfiguration' smart constructor.
data ExperienceConfiguration = ExperienceConfiguration'
  { -- | The identifiers of your data sources and FAQs. Or, you can specify that
    -- you want to use documents indexed via the @BatchPutDocument@ API. This
    -- is the content you want to use for your Amazon Kendra experience.
    contentSourceConfiguration :: Prelude.Maybe ContentSourceConfiguration,
    -- | The IAM Identity Center field name that contains the identifiers of your
    -- users, such as their emails.
    userIdentityConfiguration :: Prelude.Maybe UserIdentityConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperienceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentSourceConfiguration', 'experienceConfiguration_contentSourceConfiguration' - The identifiers of your data sources and FAQs. Or, you can specify that
-- you want to use documents indexed via the @BatchPutDocument@ API. This
-- is the content you want to use for your Amazon Kendra experience.
--
-- 'userIdentityConfiguration', 'experienceConfiguration_userIdentityConfiguration' - The IAM Identity Center field name that contains the identifiers of your
-- users, such as their emails.
newExperienceConfiguration ::
  ExperienceConfiguration
newExperienceConfiguration =
  ExperienceConfiguration'
    { contentSourceConfiguration =
        Prelude.Nothing,
      userIdentityConfiguration = Prelude.Nothing
    }

-- | The identifiers of your data sources and FAQs. Or, you can specify that
-- you want to use documents indexed via the @BatchPutDocument@ API. This
-- is the content you want to use for your Amazon Kendra experience.
experienceConfiguration_contentSourceConfiguration :: Lens.Lens' ExperienceConfiguration (Prelude.Maybe ContentSourceConfiguration)
experienceConfiguration_contentSourceConfiguration = Lens.lens (\ExperienceConfiguration' {contentSourceConfiguration} -> contentSourceConfiguration) (\s@ExperienceConfiguration' {} a -> s {contentSourceConfiguration = a} :: ExperienceConfiguration)

-- | The IAM Identity Center field name that contains the identifiers of your
-- users, such as their emails.
experienceConfiguration_userIdentityConfiguration :: Lens.Lens' ExperienceConfiguration (Prelude.Maybe UserIdentityConfiguration)
experienceConfiguration_userIdentityConfiguration = Lens.lens (\ExperienceConfiguration' {userIdentityConfiguration} -> userIdentityConfiguration) (\s@ExperienceConfiguration' {} a -> s {userIdentityConfiguration = a} :: ExperienceConfiguration)

instance Core.FromJSON ExperienceConfiguration where
  parseJSON =
    Core.withObject
      "ExperienceConfiguration"
      ( \x ->
          ExperienceConfiguration'
            Prelude.<$> (x Core..:? "ContentSourceConfiguration")
            Prelude.<*> (x Core..:? "UserIdentityConfiguration")
      )

instance Prelude.Hashable ExperienceConfiguration where
  hashWithSalt _salt ExperienceConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` contentSourceConfiguration
      `Prelude.hashWithSalt` userIdentityConfiguration

instance Prelude.NFData ExperienceConfiguration where
  rnf ExperienceConfiguration' {..} =
    Prelude.rnf contentSourceConfiguration
      `Prelude.seq` Prelude.rnf userIdentityConfiguration

instance Core.ToJSON ExperienceConfiguration where
  toJSON ExperienceConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ContentSourceConfiguration" Core..=)
              Prelude.<$> contentSourceConfiguration,
            ("UserIdentityConfiguration" Core..=)
              Prelude.<$> userIdentityConfiguration
          ]
      )
