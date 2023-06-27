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
-- Module      : Amazonka.ElasticBeanstalk.Types.ApplicationVersionLifecycleConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.ApplicationVersionLifecycleConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types.MaxAgeRule
import Amazonka.ElasticBeanstalk.Types.MaxCountRule
import qualified Amazonka.Prelude as Prelude

-- | The application version lifecycle settings for an application. Defines
-- the rules that Elastic Beanstalk applies to an application\'s versions
-- in order to avoid hitting the per-region limit for application versions.
--
-- When Elastic Beanstalk deletes an application version from its database,
-- you can no longer deploy that version to an environment. The source
-- bundle remains in S3 unless you configure the rule to delete it.
--
-- /See:/ 'newApplicationVersionLifecycleConfig' smart constructor.
data ApplicationVersionLifecycleConfig = ApplicationVersionLifecycleConfig'
  { -- | Specify a max age rule to restrict the length of time that application
    -- versions are retained for an application.
    maxAgeRule :: Prelude.Maybe MaxAgeRule,
    -- | Specify a max count rule to restrict the number of application versions
    -- that are retained for an application.
    maxCountRule :: Prelude.Maybe MaxCountRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationVersionLifecycleConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxAgeRule', 'applicationVersionLifecycleConfig_maxAgeRule' - Specify a max age rule to restrict the length of time that application
-- versions are retained for an application.
--
-- 'maxCountRule', 'applicationVersionLifecycleConfig_maxCountRule' - Specify a max count rule to restrict the number of application versions
-- that are retained for an application.
newApplicationVersionLifecycleConfig ::
  ApplicationVersionLifecycleConfig
newApplicationVersionLifecycleConfig =
  ApplicationVersionLifecycleConfig'
    { maxAgeRule =
        Prelude.Nothing,
      maxCountRule = Prelude.Nothing
    }

-- | Specify a max age rule to restrict the length of time that application
-- versions are retained for an application.
applicationVersionLifecycleConfig_maxAgeRule :: Lens.Lens' ApplicationVersionLifecycleConfig (Prelude.Maybe MaxAgeRule)
applicationVersionLifecycleConfig_maxAgeRule = Lens.lens (\ApplicationVersionLifecycleConfig' {maxAgeRule} -> maxAgeRule) (\s@ApplicationVersionLifecycleConfig' {} a -> s {maxAgeRule = a} :: ApplicationVersionLifecycleConfig)

-- | Specify a max count rule to restrict the number of application versions
-- that are retained for an application.
applicationVersionLifecycleConfig_maxCountRule :: Lens.Lens' ApplicationVersionLifecycleConfig (Prelude.Maybe MaxCountRule)
applicationVersionLifecycleConfig_maxCountRule = Lens.lens (\ApplicationVersionLifecycleConfig' {maxCountRule} -> maxCountRule) (\s@ApplicationVersionLifecycleConfig' {} a -> s {maxCountRule = a} :: ApplicationVersionLifecycleConfig)

instance
  Data.FromXML
    ApplicationVersionLifecycleConfig
  where
  parseXML x =
    ApplicationVersionLifecycleConfig'
      Prelude.<$> (x Data..@? "MaxAgeRule")
      Prelude.<*> (x Data..@? "MaxCountRule")

instance
  Prelude.Hashable
    ApplicationVersionLifecycleConfig
  where
  hashWithSalt
    _salt
    ApplicationVersionLifecycleConfig' {..} =
      _salt
        `Prelude.hashWithSalt` maxAgeRule
        `Prelude.hashWithSalt` maxCountRule

instance
  Prelude.NFData
    ApplicationVersionLifecycleConfig
  where
  rnf ApplicationVersionLifecycleConfig' {..} =
    Prelude.rnf maxAgeRule
      `Prelude.seq` Prelude.rnf maxCountRule

instance
  Data.ToQuery
    ApplicationVersionLifecycleConfig
  where
  toQuery ApplicationVersionLifecycleConfig' {..} =
    Prelude.mconcat
      [ "MaxAgeRule" Data.=: maxAgeRule,
        "MaxCountRule" Data.=: maxCountRule
      ]
