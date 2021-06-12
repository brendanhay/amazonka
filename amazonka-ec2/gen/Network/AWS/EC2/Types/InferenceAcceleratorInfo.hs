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
-- Module      : Network.AWS.EC2.Types.InferenceAcceleratorInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InferenceAcceleratorInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InferenceDeviceInfo
import qualified Network.AWS.Lens as Lens

-- | Describes the Inference accelerators for the instance type.
--
-- /See:/ 'newInferenceAcceleratorInfo' smart constructor.
data InferenceAcceleratorInfo = InferenceAcceleratorInfo'
  { -- | Describes the Inference accelerators for the instance type.
    accelerators :: Core.Maybe [InferenceDeviceInfo]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing
    }

-- | Describes the Inference accelerators for the instance type.
inferenceAcceleratorInfo_accelerators :: Lens.Lens' InferenceAcceleratorInfo (Core.Maybe [InferenceDeviceInfo])
inferenceAcceleratorInfo_accelerators = Lens.lens (\InferenceAcceleratorInfo' {accelerators} -> accelerators) (\s@InferenceAcceleratorInfo' {} a -> s {accelerators = a} :: InferenceAcceleratorInfo) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML InferenceAcceleratorInfo where
  parseXML x =
    InferenceAcceleratorInfo'
      Core.<$> ( x Core..@? "accelerators" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )

instance Core.Hashable InferenceAcceleratorInfo

instance Core.NFData InferenceAcceleratorInfo
