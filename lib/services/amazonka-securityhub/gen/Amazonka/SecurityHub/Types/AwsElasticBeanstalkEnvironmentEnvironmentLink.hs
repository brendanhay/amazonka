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
-- Module      : Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentEnvironmentLink
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentEnvironmentLink where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a link to another environment that is in the
-- same group.
--
-- /See:/ 'newAwsElasticBeanstalkEnvironmentEnvironmentLink' smart constructor.
data AwsElasticBeanstalkEnvironmentEnvironmentLink = AwsElasticBeanstalkEnvironmentEnvironmentLink'
  { -- | The name of the environment link.
    linkName :: Prelude.Maybe Prelude.Text,
    -- | The name of the linked environment.
    environmentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElasticBeanstalkEnvironmentEnvironmentLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'linkName', 'awsElasticBeanstalkEnvironmentEnvironmentLink_linkName' - The name of the environment link.
--
-- 'environmentName', 'awsElasticBeanstalkEnvironmentEnvironmentLink_environmentName' - The name of the linked environment.
newAwsElasticBeanstalkEnvironmentEnvironmentLink ::
  AwsElasticBeanstalkEnvironmentEnvironmentLink
newAwsElasticBeanstalkEnvironmentEnvironmentLink =
  AwsElasticBeanstalkEnvironmentEnvironmentLink'
    { linkName =
        Prelude.Nothing,
      environmentName =
        Prelude.Nothing
    }

-- | The name of the environment link.
awsElasticBeanstalkEnvironmentEnvironmentLink_linkName :: Lens.Lens' AwsElasticBeanstalkEnvironmentEnvironmentLink (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentEnvironmentLink_linkName = Lens.lens (\AwsElasticBeanstalkEnvironmentEnvironmentLink' {linkName} -> linkName) (\s@AwsElasticBeanstalkEnvironmentEnvironmentLink' {} a -> s {linkName = a} :: AwsElasticBeanstalkEnvironmentEnvironmentLink)

-- | The name of the linked environment.
awsElasticBeanstalkEnvironmentEnvironmentLink_environmentName :: Lens.Lens' AwsElasticBeanstalkEnvironmentEnvironmentLink (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentEnvironmentLink_environmentName = Lens.lens (\AwsElasticBeanstalkEnvironmentEnvironmentLink' {environmentName} -> environmentName) (\s@AwsElasticBeanstalkEnvironmentEnvironmentLink' {} a -> s {environmentName = a} :: AwsElasticBeanstalkEnvironmentEnvironmentLink)

instance
  Core.FromJSON
    AwsElasticBeanstalkEnvironmentEnvironmentLink
  where
  parseJSON =
    Core.withObject
      "AwsElasticBeanstalkEnvironmentEnvironmentLink"
      ( \x ->
          AwsElasticBeanstalkEnvironmentEnvironmentLink'
            Prelude.<$> (x Core..:? "LinkName")
              Prelude.<*> (x Core..:? "EnvironmentName")
      )

instance
  Prelude.Hashable
    AwsElasticBeanstalkEnvironmentEnvironmentLink
  where
  hashWithSalt
    salt'
    AwsElasticBeanstalkEnvironmentEnvironmentLink' {..} =
      salt' `Prelude.hashWithSalt` environmentName
        `Prelude.hashWithSalt` linkName

instance
  Prelude.NFData
    AwsElasticBeanstalkEnvironmentEnvironmentLink
  where
  rnf
    AwsElasticBeanstalkEnvironmentEnvironmentLink' {..} =
      Prelude.rnf linkName
        `Prelude.seq` Prelude.rnf environmentName

instance
  Core.ToJSON
    AwsElasticBeanstalkEnvironmentEnvironmentLink
  where
  toJSON
    AwsElasticBeanstalkEnvironmentEnvironmentLink' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("LinkName" Core..=) Prelude.<$> linkName,
              ("EnvironmentName" Core..=)
                Prelude.<$> environmentName
            ]
        )
