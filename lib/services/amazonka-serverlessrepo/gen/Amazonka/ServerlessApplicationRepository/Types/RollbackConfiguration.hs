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
-- Module      : Amazonka.ServerlessApplicationRepository.Types.RollbackConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServerlessApplicationRepository.Types.RollbackConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServerlessApplicationRepository.Types.RollbackTrigger

-- | This property corresponds to the /AWS CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/RollbackConfiguration RollbackConfiguration>/
-- Data Type.
--
-- /See:/ 'newRollbackConfiguration' smart constructor.
data RollbackConfiguration = RollbackConfiguration'
  { -- | This property corresponds to the content of the same name for the /AWS
    -- CloudFormation
    -- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/RollbackConfiguration RollbackConfiguration>/
    -- Data Type.
    monitoringTimeInMinutes :: Prelude.Maybe Prelude.Int,
    -- | This property corresponds to the content of the same name for the /AWS
    -- CloudFormation
    -- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/RollbackConfiguration RollbackConfiguration>/
    -- Data Type.
    rollbackTriggers :: Prelude.Maybe [RollbackTrigger]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RollbackConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitoringTimeInMinutes', 'rollbackConfiguration_monitoringTimeInMinutes' - This property corresponds to the content of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/RollbackConfiguration RollbackConfiguration>/
-- Data Type.
--
-- 'rollbackTriggers', 'rollbackConfiguration_rollbackTriggers' - This property corresponds to the content of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/RollbackConfiguration RollbackConfiguration>/
-- Data Type.
newRollbackConfiguration ::
  RollbackConfiguration
newRollbackConfiguration =
  RollbackConfiguration'
    { monitoringTimeInMinutes =
        Prelude.Nothing,
      rollbackTriggers = Prelude.Nothing
    }

-- | This property corresponds to the content of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/RollbackConfiguration RollbackConfiguration>/
-- Data Type.
rollbackConfiguration_monitoringTimeInMinutes :: Lens.Lens' RollbackConfiguration (Prelude.Maybe Prelude.Int)
rollbackConfiguration_monitoringTimeInMinutes = Lens.lens (\RollbackConfiguration' {monitoringTimeInMinutes} -> monitoringTimeInMinutes) (\s@RollbackConfiguration' {} a -> s {monitoringTimeInMinutes = a} :: RollbackConfiguration)

-- | This property corresponds to the content of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/RollbackConfiguration RollbackConfiguration>/
-- Data Type.
rollbackConfiguration_rollbackTriggers :: Lens.Lens' RollbackConfiguration (Prelude.Maybe [RollbackTrigger])
rollbackConfiguration_rollbackTriggers = Lens.lens (\RollbackConfiguration' {rollbackTriggers} -> rollbackTriggers) (\s@RollbackConfiguration' {} a -> s {rollbackTriggers = a} :: RollbackConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable RollbackConfiguration where
  hashWithSalt _salt RollbackConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` monitoringTimeInMinutes
      `Prelude.hashWithSalt` rollbackTriggers

instance Prelude.NFData RollbackConfiguration where
  rnf RollbackConfiguration' {..} =
    Prelude.rnf monitoringTimeInMinutes
      `Prelude.seq` Prelude.rnf rollbackTriggers

instance Data.ToJSON RollbackConfiguration where
  toJSON RollbackConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("monitoringTimeInMinutes" Data..=)
              Prelude.<$> monitoringTimeInMinutes,
            ("rollbackTriggers" Data..=)
              Prelude.<$> rollbackTriggers
          ]
      )
