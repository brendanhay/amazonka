{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationResourceLifecycleConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationResourceLifecycleConfig where

import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionLifecycleConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The resource lifecycle configuration for an application. Defines
-- lifecycle settings for resources that belong to the application, and the
-- service role that AWS Elastic Beanstalk assumes in order to apply
-- lifecycle settings. The version lifecycle configuration defines
-- lifecycle settings for application versions.
--
-- /See:/ 'newApplicationResourceLifecycleConfig' smart constructor.
data ApplicationResourceLifecycleConfig = ApplicationResourceLifecycleConfig'
  { -- | The ARN of an IAM service role that Elastic Beanstalk has permission to
    -- assume.
    --
    -- The @ServiceRole@ property is required the first time that you provide a
    -- @VersionLifecycleConfig@ for the application in one of the supporting
    -- calls (@CreateApplication@ or @UpdateApplicationResourceLifecycle@).
    -- After you provide it once, in either one of the calls, Elastic Beanstalk
    -- persists the Service Role with the application, and you don\'t need to
    -- specify it again in subsequent @UpdateApplicationResourceLifecycle@
    -- calls. You can, however, specify it in subsequent calls to change the
    -- Service Role to another value.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | Defines lifecycle settings for application versions.
    versionLifecycleConfig :: Prelude.Maybe ApplicationVersionLifecycleConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ApplicationResourceLifecycleConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceRole', 'applicationResourceLifecycleConfig_serviceRole' - The ARN of an IAM service role that Elastic Beanstalk has permission to
-- assume.
--
-- The @ServiceRole@ property is required the first time that you provide a
-- @VersionLifecycleConfig@ for the application in one of the supporting
-- calls (@CreateApplication@ or @UpdateApplicationResourceLifecycle@).
-- After you provide it once, in either one of the calls, Elastic Beanstalk
-- persists the Service Role with the application, and you don\'t need to
-- specify it again in subsequent @UpdateApplicationResourceLifecycle@
-- calls. You can, however, specify it in subsequent calls to change the
-- Service Role to another value.
--
-- 'versionLifecycleConfig', 'applicationResourceLifecycleConfig_versionLifecycleConfig' - Defines lifecycle settings for application versions.
newApplicationResourceLifecycleConfig ::
  ApplicationResourceLifecycleConfig
newApplicationResourceLifecycleConfig =
  ApplicationResourceLifecycleConfig'
    { serviceRole =
        Prelude.Nothing,
      versionLifecycleConfig =
        Prelude.Nothing
    }

-- | The ARN of an IAM service role that Elastic Beanstalk has permission to
-- assume.
--
-- The @ServiceRole@ property is required the first time that you provide a
-- @VersionLifecycleConfig@ for the application in one of the supporting
-- calls (@CreateApplication@ or @UpdateApplicationResourceLifecycle@).
-- After you provide it once, in either one of the calls, Elastic Beanstalk
-- persists the Service Role with the application, and you don\'t need to
-- specify it again in subsequent @UpdateApplicationResourceLifecycle@
-- calls. You can, however, specify it in subsequent calls to change the
-- Service Role to another value.
applicationResourceLifecycleConfig_serviceRole :: Lens.Lens' ApplicationResourceLifecycleConfig (Prelude.Maybe Prelude.Text)
applicationResourceLifecycleConfig_serviceRole = Lens.lens (\ApplicationResourceLifecycleConfig' {serviceRole} -> serviceRole) (\s@ApplicationResourceLifecycleConfig' {} a -> s {serviceRole = a} :: ApplicationResourceLifecycleConfig)

-- | Defines lifecycle settings for application versions.
applicationResourceLifecycleConfig_versionLifecycleConfig :: Lens.Lens' ApplicationResourceLifecycleConfig (Prelude.Maybe ApplicationVersionLifecycleConfig)
applicationResourceLifecycleConfig_versionLifecycleConfig = Lens.lens (\ApplicationResourceLifecycleConfig' {versionLifecycleConfig} -> versionLifecycleConfig) (\s@ApplicationResourceLifecycleConfig' {} a -> s {versionLifecycleConfig = a} :: ApplicationResourceLifecycleConfig)

instance
  Prelude.FromXML
    ApplicationResourceLifecycleConfig
  where
  parseXML x =
    ApplicationResourceLifecycleConfig'
      Prelude.<$> (x Prelude..@? "ServiceRole")
      Prelude.<*> (x Prelude..@? "VersionLifecycleConfig")

instance
  Prelude.Hashable
    ApplicationResourceLifecycleConfig

instance
  Prelude.NFData
    ApplicationResourceLifecycleConfig

instance
  Prelude.ToQuery
    ApplicationResourceLifecycleConfig
  where
  toQuery ApplicationResourceLifecycleConfig' {..} =
    Prelude.mconcat
      [ "ServiceRole" Prelude.=: serviceRole,
        "VersionLifecycleConfig"
          Prelude.=: versionLifecycleConfig
      ]
