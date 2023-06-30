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
-- Module      : Amazonka.EC2.Types.InferenceAcceleratorInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InferenceAcceleratorInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InferenceDeviceInfo
import qualified Amazonka.Prelude as Prelude

-- | Describes the Inference accelerators for the instance type.
--
-- /See:/ 'newInferenceAcceleratorInfo' smart constructor.
data InferenceAcceleratorInfo = InferenceAcceleratorInfo'
  { -- | Describes the Inference accelerators for the instance type.
    accelerators :: Prelude.Maybe [InferenceDeviceInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferenceAcceleratorInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accelerators', 'inferenceAcceleratorInfo_accelerators' - Describes the Inference accelerators for the instance type.
newInferenceAcceleratorInfo ::
  InferenceAcceleratorInfo
newInferenceAcceleratorInfo =
  InferenceAcceleratorInfo'
    { accelerators =
        Prelude.Nothing
    }

-- | Describes the Inference accelerators for the instance type.
inferenceAcceleratorInfo_accelerators :: Lens.Lens' InferenceAcceleratorInfo (Prelude.Maybe [InferenceDeviceInfo])
inferenceAcceleratorInfo_accelerators = Lens.lens (\InferenceAcceleratorInfo' {accelerators} -> accelerators) (\s@InferenceAcceleratorInfo' {} a -> s {accelerators = a} :: InferenceAcceleratorInfo) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML InferenceAcceleratorInfo where
  parseXML x =
    InferenceAcceleratorInfo'
      Prelude.<$> ( x
                      Data..@? "accelerators"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance Prelude.Hashable InferenceAcceleratorInfo where
  hashWithSalt _salt InferenceAcceleratorInfo' {..} =
    _salt `Prelude.hashWithSalt` accelerators

instance Prelude.NFData InferenceAcceleratorInfo where
  rnf InferenceAcceleratorInfo' {..} =
    Prelude.rnf accelerators
