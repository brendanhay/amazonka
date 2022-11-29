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
-- Module      : Amazonka.ServerlessApplicationRepository.Types.RollbackTrigger
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServerlessApplicationRepository.Types.RollbackTrigger where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | This property corresponds to the /AWS CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/RollbackTrigger RollbackTrigger>/
-- Data Type.
--
-- /See:/ 'newRollbackTrigger' smart constructor.
data RollbackTrigger = RollbackTrigger'
  { -- | This property corresponds to the content of the same name for the /AWS
    -- CloudFormation
    -- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/RollbackTrigger RollbackTrigger>/
    -- Data Type.
    type' :: Prelude.Text,
    -- | This property corresponds to the content of the same name for the /AWS
    -- CloudFormation
    -- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/RollbackTrigger RollbackTrigger>/
    -- Data Type.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RollbackTrigger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'rollbackTrigger_type' - This property corresponds to the content of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/RollbackTrigger RollbackTrigger>/
-- Data Type.
--
-- 'arn', 'rollbackTrigger_arn' - This property corresponds to the content of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/RollbackTrigger RollbackTrigger>/
-- Data Type.
newRollbackTrigger ::
  -- | 'type''
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  RollbackTrigger
newRollbackTrigger pType_ pArn_ =
  RollbackTrigger' {type' = pType_, arn = pArn_}

-- | This property corresponds to the content of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/RollbackTrigger RollbackTrigger>/
-- Data Type.
rollbackTrigger_type :: Lens.Lens' RollbackTrigger Prelude.Text
rollbackTrigger_type = Lens.lens (\RollbackTrigger' {type'} -> type') (\s@RollbackTrigger' {} a -> s {type' = a} :: RollbackTrigger)

-- | This property corresponds to the content of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/RollbackTrigger RollbackTrigger>/
-- Data Type.
rollbackTrigger_arn :: Lens.Lens' RollbackTrigger Prelude.Text
rollbackTrigger_arn = Lens.lens (\RollbackTrigger' {arn} -> arn) (\s@RollbackTrigger' {} a -> s {arn = a} :: RollbackTrigger)

instance Prelude.Hashable RollbackTrigger where
  hashWithSalt _salt RollbackTrigger' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` arn

instance Prelude.NFData RollbackTrigger where
  rnf RollbackTrigger' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf arn

instance Core.ToJSON RollbackTrigger where
  toJSON RollbackTrigger' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("type" Core..= type'),
            Prelude.Just ("arn" Core..= arn)
          ]
      )
