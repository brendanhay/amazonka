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
-- Module      : Network.AWS.SageMaker.Types.RegisterModelStepMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.RegisterModelStepMetadata where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Metadata for a register model job step.
--
-- /See:/ 'newRegisterModelStepMetadata' smart constructor.
data RegisterModelStepMetadata = RegisterModelStepMetadata'
  { -- | The Amazon Resource Name (ARN) of the model package.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegisterModelStepMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'registerModelStepMetadata_arn' - The Amazon Resource Name (ARN) of the model package.
newRegisterModelStepMetadata ::
  RegisterModelStepMetadata
newRegisterModelStepMetadata =
  RegisterModelStepMetadata' {arn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the model package.
registerModelStepMetadata_arn :: Lens.Lens' RegisterModelStepMetadata (Prelude.Maybe Prelude.Text)
registerModelStepMetadata_arn = Lens.lens (\RegisterModelStepMetadata' {arn} -> arn) (\s@RegisterModelStepMetadata' {} a -> s {arn = a} :: RegisterModelStepMetadata)

instance Prelude.FromJSON RegisterModelStepMetadata where
  parseJSON =
    Prelude.withObject
      "RegisterModelStepMetadata"
      ( \x ->
          RegisterModelStepMetadata'
            Prelude.<$> (x Prelude..:? "Arn")
      )

instance Prelude.Hashable RegisterModelStepMetadata

instance Prelude.NFData RegisterModelStepMetadata
