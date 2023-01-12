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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataHibernationOptionsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataHibernationOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies whether your Amazon EC2 instance is configured for
-- hibernation.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataHibernationOptionsDetails' smart constructor.
data AwsEc2LaunchTemplateDataHibernationOptionsDetails = AwsEc2LaunchTemplateDataHibernationOptionsDetails'
  { -- | If you set this parameter to @true@, the instance is enabled for
    -- hibernation.
    configured :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataHibernationOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configured', 'awsEc2LaunchTemplateDataHibernationOptionsDetails_configured' - If you set this parameter to @true@, the instance is enabled for
-- hibernation.
newAwsEc2LaunchTemplateDataHibernationOptionsDetails ::
  AwsEc2LaunchTemplateDataHibernationOptionsDetails
newAwsEc2LaunchTemplateDataHibernationOptionsDetails =
  AwsEc2LaunchTemplateDataHibernationOptionsDetails'
    { configured =
        Prelude.Nothing
    }

-- | If you set this parameter to @true@, the instance is enabled for
-- hibernation.
awsEc2LaunchTemplateDataHibernationOptionsDetails_configured :: Lens.Lens' AwsEc2LaunchTemplateDataHibernationOptionsDetails (Prelude.Maybe Prelude.Bool)
awsEc2LaunchTemplateDataHibernationOptionsDetails_configured = Lens.lens (\AwsEc2LaunchTemplateDataHibernationOptionsDetails' {configured} -> configured) (\s@AwsEc2LaunchTemplateDataHibernationOptionsDetails' {} a -> s {configured = a} :: AwsEc2LaunchTemplateDataHibernationOptionsDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataHibernationOptionsDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataHibernationOptionsDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataHibernationOptionsDetails'
            Prelude.<$> (x Data..:? "Configured")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataHibernationOptionsDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataHibernationOptionsDetails' {..} =
      _salt `Prelude.hashWithSalt` configured

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataHibernationOptionsDetails
  where
  rnf
    AwsEc2LaunchTemplateDataHibernationOptionsDetails' {..} =
      Prelude.rnf configured

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataHibernationOptionsDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataHibernationOptionsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("Configured" Data..=) Prelude.<$> configured]
        )
