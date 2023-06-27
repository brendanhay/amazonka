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
-- Module      : Amazonka.ElasticBeanstalk.Types.EnvironmentLink
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.EnvironmentLink where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A link to another environment, defined in the environment\'s manifest.
-- Links provide connection information in system properties that can be
-- used to connect to another environment in the same group. See
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)>
-- for details.
--
-- /See:/ 'newEnvironmentLink' smart constructor.
data EnvironmentLink = EnvironmentLink'
  { -- | The name of the linked environment (the dependency).
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | The name of the link.
    linkName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentName', 'environmentLink_environmentName' - The name of the linked environment (the dependency).
--
-- 'linkName', 'environmentLink_linkName' - The name of the link.
newEnvironmentLink ::
  EnvironmentLink
newEnvironmentLink =
  EnvironmentLink'
    { environmentName = Prelude.Nothing,
      linkName = Prelude.Nothing
    }

-- | The name of the linked environment (the dependency).
environmentLink_environmentName :: Lens.Lens' EnvironmentLink (Prelude.Maybe Prelude.Text)
environmentLink_environmentName = Lens.lens (\EnvironmentLink' {environmentName} -> environmentName) (\s@EnvironmentLink' {} a -> s {environmentName = a} :: EnvironmentLink)

-- | The name of the link.
environmentLink_linkName :: Lens.Lens' EnvironmentLink (Prelude.Maybe Prelude.Text)
environmentLink_linkName = Lens.lens (\EnvironmentLink' {linkName} -> linkName) (\s@EnvironmentLink' {} a -> s {linkName = a} :: EnvironmentLink)

instance Data.FromXML EnvironmentLink where
  parseXML x =
    EnvironmentLink'
      Prelude.<$> (x Data..@? "EnvironmentName")
      Prelude.<*> (x Data..@? "LinkName")

instance Prelude.Hashable EnvironmentLink where
  hashWithSalt _salt EnvironmentLink' {..} =
    _salt
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` linkName

instance Prelude.NFData EnvironmentLink where
  rnf EnvironmentLink' {..} =
    Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf linkName
