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
-- Module      : Amazonka.ElasticBeanstalk.Types.ResourceQuotas
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.ResourceQuotas where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types.ResourceQuota
import qualified Amazonka.Prelude as Prelude

-- | A set of per-resource AWS Elastic Beanstalk quotas associated with an
-- AWS account. They reflect Elastic Beanstalk resource limits for this
-- account.
--
-- /See:/ 'newResourceQuotas' smart constructor.
data ResourceQuotas = ResourceQuotas'
  { -- | The quota for applications in the AWS account.
    applicationQuota :: Prelude.Maybe ResourceQuota,
    -- | The quota for application versions in the AWS account.
    applicationVersionQuota :: Prelude.Maybe ResourceQuota,
    -- | The quota for configuration templates in the AWS account.
    configurationTemplateQuota :: Prelude.Maybe ResourceQuota,
    -- | The quota for custom platforms in the AWS account.
    customPlatformQuota :: Prelude.Maybe ResourceQuota,
    -- | The quota for environments in the AWS account.
    environmentQuota :: Prelude.Maybe ResourceQuota
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceQuotas' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationQuota', 'resourceQuotas_applicationQuota' - The quota for applications in the AWS account.
--
-- 'applicationVersionQuota', 'resourceQuotas_applicationVersionQuota' - The quota for application versions in the AWS account.
--
-- 'configurationTemplateQuota', 'resourceQuotas_configurationTemplateQuota' - The quota for configuration templates in the AWS account.
--
-- 'customPlatformQuota', 'resourceQuotas_customPlatformQuota' - The quota for custom platforms in the AWS account.
--
-- 'environmentQuota', 'resourceQuotas_environmentQuota' - The quota for environments in the AWS account.
newResourceQuotas ::
  ResourceQuotas
newResourceQuotas =
  ResourceQuotas'
    { applicationQuota = Prelude.Nothing,
      applicationVersionQuota = Prelude.Nothing,
      configurationTemplateQuota = Prelude.Nothing,
      customPlatformQuota = Prelude.Nothing,
      environmentQuota = Prelude.Nothing
    }

-- | The quota for applications in the AWS account.
resourceQuotas_applicationQuota :: Lens.Lens' ResourceQuotas (Prelude.Maybe ResourceQuota)
resourceQuotas_applicationQuota = Lens.lens (\ResourceQuotas' {applicationQuota} -> applicationQuota) (\s@ResourceQuotas' {} a -> s {applicationQuota = a} :: ResourceQuotas)

-- | The quota for application versions in the AWS account.
resourceQuotas_applicationVersionQuota :: Lens.Lens' ResourceQuotas (Prelude.Maybe ResourceQuota)
resourceQuotas_applicationVersionQuota = Lens.lens (\ResourceQuotas' {applicationVersionQuota} -> applicationVersionQuota) (\s@ResourceQuotas' {} a -> s {applicationVersionQuota = a} :: ResourceQuotas)

-- | The quota for configuration templates in the AWS account.
resourceQuotas_configurationTemplateQuota :: Lens.Lens' ResourceQuotas (Prelude.Maybe ResourceQuota)
resourceQuotas_configurationTemplateQuota = Lens.lens (\ResourceQuotas' {configurationTemplateQuota} -> configurationTemplateQuota) (\s@ResourceQuotas' {} a -> s {configurationTemplateQuota = a} :: ResourceQuotas)

-- | The quota for custom platforms in the AWS account.
resourceQuotas_customPlatformQuota :: Lens.Lens' ResourceQuotas (Prelude.Maybe ResourceQuota)
resourceQuotas_customPlatformQuota = Lens.lens (\ResourceQuotas' {customPlatformQuota} -> customPlatformQuota) (\s@ResourceQuotas' {} a -> s {customPlatformQuota = a} :: ResourceQuotas)

-- | The quota for environments in the AWS account.
resourceQuotas_environmentQuota :: Lens.Lens' ResourceQuotas (Prelude.Maybe ResourceQuota)
resourceQuotas_environmentQuota = Lens.lens (\ResourceQuotas' {environmentQuota} -> environmentQuota) (\s@ResourceQuotas' {} a -> s {environmentQuota = a} :: ResourceQuotas)

instance Data.FromXML ResourceQuotas where
  parseXML x =
    ResourceQuotas'
      Prelude.<$> (x Data..@? "ApplicationQuota")
      Prelude.<*> (x Data..@? "ApplicationVersionQuota")
      Prelude.<*> (x Data..@? "ConfigurationTemplateQuota")
      Prelude.<*> (x Data..@? "CustomPlatformQuota")
      Prelude.<*> (x Data..@? "EnvironmentQuota")

instance Prelude.Hashable ResourceQuotas where
  hashWithSalt _salt ResourceQuotas' {..} =
    _salt
      `Prelude.hashWithSalt` applicationQuota
      `Prelude.hashWithSalt` applicationVersionQuota
      `Prelude.hashWithSalt` configurationTemplateQuota
      `Prelude.hashWithSalt` customPlatformQuota
      `Prelude.hashWithSalt` environmentQuota

instance Prelude.NFData ResourceQuotas where
  rnf ResourceQuotas' {..} =
    Prelude.rnf applicationQuota
      `Prelude.seq` Prelude.rnf applicationVersionQuota
      `Prelude.seq` Prelude.rnf configurationTemplateQuota
      `Prelude.seq` Prelude.rnf customPlatformQuota
      `Prelude.seq` Prelude.rnf environmentQuota
