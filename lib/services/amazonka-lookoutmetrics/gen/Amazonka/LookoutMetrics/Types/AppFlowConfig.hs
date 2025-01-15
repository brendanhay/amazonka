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
-- Module      : Amazonka.LookoutMetrics.Types.AppFlowConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.AppFlowConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about an Amazon AppFlow flow datasource.
--
-- /See:/ 'newAppFlowConfig' smart constructor.
data AppFlowConfig = AppFlowConfig'
  { -- | name of the flow.
    flowName :: Prelude.Maybe Prelude.Text,
    -- | An IAM role that gives Amazon Lookout for Metrics permission to access
    -- the flow.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppFlowConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowName', 'appFlowConfig_flowName' - name of the flow.
--
-- 'roleArn', 'appFlowConfig_roleArn' - An IAM role that gives Amazon Lookout for Metrics permission to access
-- the flow.
newAppFlowConfig ::
  AppFlowConfig
newAppFlowConfig =
  AppFlowConfig'
    { flowName = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | name of the flow.
appFlowConfig_flowName :: Lens.Lens' AppFlowConfig (Prelude.Maybe Prelude.Text)
appFlowConfig_flowName = Lens.lens (\AppFlowConfig' {flowName} -> flowName) (\s@AppFlowConfig' {} a -> s {flowName = a} :: AppFlowConfig)

-- | An IAM role that gives Amazon Lookout for Metrics permission to access
-- the flow.
appFlowConfig_roleArn :: Lens.Lens' AppFlowConfig (Prelude.Maybe Prelude.Text)
appFlowConfig_roleArn = Lens.lens (\AppFlowConfig' {roleArn} -> roleArn) (\s@AppFlowConfig' {} a -> s {roleArn = a} :: AppFlowConfig)

instance Data.FromJSON AppFlowConfig where
  parseJSON =
    Data.withObject
      "AppFlowConfig"
      ( \x ->
          AppFlowConfig'
            Prelude.<$> (x Data..:? "FlowName")
            Prelude.<*> (x Data..:? "RoleArn")
      )

instance Prelude.Hashable AppFlowConfig where
  hashWithSalt _salt AppFlowConfig' {..} =
    _salt
      `Prelude.hashWithSalt` flowName
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData AppFlowConfig where
  rnf AppFlowConfig' {..} =
    Prelude.rnf flowName `Prelude.seq`
      Prelude.rnf roleArn

instance Data.ToJSON AppFlowConfig where
  toJSON AppFlowConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FlowName" Data..=) Prelude.<$> flowName,
            ("RoleArn" Data..=) Prelude.<$> roleArn
          ]
      )
