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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentEnvironmentLink where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a link to another environment that is in the
-- same group.
--
-- /See:/ 'newAwsElasticBeanstalkEnvironmentEnvironmentLink' smart constructor.
data AwsElasticBeanstalkEnvironmentEnvironmentLink = AwsElasticBeanstalkEnvironmentEnvironmentLink'
  { -- | The name of the linked environment.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | The name of the environment link.
    linkName :: Prelude.Maybe Prelude.Text
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
-- 'environmentName', 'awsElasticBeanstalkEnvironmentEnvironmentLink_environmentName' - The name of the linked environment.
--
-- 'linkName', 'awsElasticBeanstalkEnvironmentEnvironmentLink_linkName' - The name of the environment link.
newAwsElasticBeanstalkEnvironmentEnvironmentLink ::
  AwsElasticBeanstalkEnvironmentEnvironmentLink
newAwsElasticBeanstalkEnvironmentEnvironmentLink =
  AwsElasticBeanstalkEnvironmentEnvironmentLink'
    { environmentName =
        Prelude.Nothing,
      linkName = Prelude.Nothing
    }

-- | The name of the linked environment.
awsElasticBeanstalkEnvironmentEnvironmentLink_environmentName :: Lens.Lens' AwsElasticBeanstalkEnvironmentEnvironmentLink (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentEnvironmentLink_environmentName = Lens.lens (\AwsElasticBeanstalkEnvironmentEnvironmentLink' {environmentName} -> environmentName) (\s@AwsElasticBeanstalkEnvironmentEnvironmentLink' {} a -> s {environmentName = a} :: AwsElasticBeanstalkEnvironmentEnvironmentLink)

-- | The name of the environment link.
awsElasticBeanstalkEnvironmentEnvironmentLink_linkName :: Lens.Lens' AwsElasticBeanstalkEnvironmentEnvironmentLink (Prelude.Maybe Prelude.Text)
awsElasticBeanstalkEnvironmentEnvironmentLink_linkName = Lens.lens (\AwsElasticBeanstalkEnvironmentEnvironmentLink' {linkName} -> linkName) (\s@AwsElasticBeanstalkEnvironmentEnvironmentLink' {} a -> s {linkName = a} :: AwsElasticBeanstalkEnvironmentEnvironmentLink)

instance
  Data.FromJSON
    AwsElasticBeanstalkEnvironmentEnvironmentLink
  where
  parseJSON =
    Data.withObject
      "AwsElasticBeanstalkEnvironmentEnvironmentLink"
      ( \x ->
          AwsElasticBeanstalkEnvironmentEnvironmentLink'
            Prelude.<$> (x Data..:? "EnvironmentName")
              Prelude.<*> (x Data..:? "LinkName")
      )

instance
  Prelude.Hashable
    AwsElasticBeanstalkEnvironmentEnvironmentLink
  where
  hashWithSalt
    _salt
    AwsElasticBeanstalkEnvironmentEnvironmentLink' {..} =
      _salt `Prelude.hashWithSalt` environmentName
        `Prelude.hashWithSalt` linkName

instance
  Prelude.NFData
    AwsElasticBeanstalkEnvironmentEnvironmentLink
  where
  rnf
    AwsElasticBeanstalkEnvironmentEnvironmentLink' {..} =
      Prelude.rnf environmentName
        `Prelude.seq` Prelude.rnf linkName

instance
  Data.ToJSON
    AwsElasticBeanstalkEnvironmentEnvironmentLink
  where
  toJSON
    AwsElasticBeanstalkEnvironmentEnvironmentLink' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("EnvironmentName" Data..=)
                Prelude.<$> environmentName,
              ("LinkName" Data..=) Prelude.<$> linkName
            ]
        )
