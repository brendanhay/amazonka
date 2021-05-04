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
-- Module      : Network.AWS.EC2.Types.InferenceAcceleratorInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InferenceAcceleratorInfo where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InferenceDeviceInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the Inference accelerators for the instance type.
--
-- /See:/ 'newInferenceAcceleratorInfo' smart constructor.
data InferenceAcceleratorInfo = InferenceAcceleratorInfo'
  { -- | Describes the Inference accelerators for the instance type.
    accelerators :: Prelude.Maybe [InferenceDeviceInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
inferenceAcceleratorInfo_accelerators = Lens.lens (\InferenceAcceleratorInfo' {accelerators} -> accelerators) (\s@InferenceAcceleratorInfo' {} a -> s {accelerators = a} :: InferenceAcceleratorInfo) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML InferenceAcceleratorInfo where
  parseXML x =
    InferenceAcceleratorInfo'
      Prelude.<$> ( x Prelude..@? "accelerators"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )

instance Prelude.Hashable InferenceAcceleratorInfo

instance Prelude.NFData InferenceAcceleratorInfo
