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
-- Module      : Network.AWS.EC2.Types.FpgaImageState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaImageState where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FpgaImageStateCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the state of the bitstream generation process for an Amazon
-- FPGA image (AFI).
--
-- /See:/ 'newFpgaImageState' smart constructor.
data FpgaImageState = FpgaImageState'
  { -- | If the state is @failed@, this is the error message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The state. The following are the possible values:
    --
    -- -   @pending@ - AFI bitstream generation is in progress.
    --
    -- -   @available@ - The AFI is available for use.
    --
    -- -   @failed@ - AFI bitstream generation failed.
    --
    -- -   @unavailable@ - The AFI is no longer available for use.
    code :: Prelude.Maybe FpgaImageStateCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FpgaImageState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'fpgaImageState_message' - If the state is @failed@, this is the error message.
--
-- 'code', 'fpgaImageState_code' - The state. The following are the possible values:
--
-- -   @pending@ - AFI bitstream generation is in progress.
--
-- -   @available@ - The AFI is available for use.
--
-- -   @failed@ - AFI bitstream generation failed.
--
-- -   @unavailable@ - The AFI is no longer available for use.
newFpgaImageState ::
  FpgaImageState
newFpgaImageState =
  FpgaImageState'
    { message = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | If the state is @failed@, this is the error message.
fpgaImageState_message :: Lens.Lens' FpgaImageState (Prelude.Maybe Prelude.Text)
fpgaImageState_message = Lens.lens (\FpgaImageState' {message} -> message) (\s@FpgaImageState' {} a -> s {message = a} :: FpgaImageState)

-- | The state. The following are the possible values:
--
-- -   @pending@ - AFI bitstream generation is in progress.
--
-- -   @available@ - The AFI is available for use.
--
-- -   @failed@ - AFI bitstream generation failed.
--
-- -   @unavailable@ - The AFI is no longer available for use.
fpgaImageState_code :: Lens.Lens' FpgaImageState (Prelude.Maybe FpgaImageStateCode)
fpgaImageState_code = Lens.lens (\FpgaImageState' {code} -> code) (\s@FpgaImageState' {} a -> s {code = a} :: FpgaImageState)

instance Prelude.FromXML FpgaImageState where
  parseXML x =
    FpgaImageState'
      Prelude.<$> (x Prelude..@? "message")
      Prelude.<*> (x Prelude..@? "code")

instance Prelude.Hashable FpgaImageState

instance Prelude.NFData FpgaImageState
