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
-- Module      : Amazonka.EC2.Types.FpgaImageState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FpgaImageState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.FpgaImageStateCode
import qualified Amazonka.Prelude as Prelude

-- | Describes the state of the bitstream generation process for an Amazon
-- FPGA image (AFI).
--
-- /See:/ 'newFpgaImageState' smart constructor.
data FpgaImageState = FpgaImageState'
  { -- | The state. The following are the possible values:
    --
    -- -   @pending@ - AFI bitstream generation is in progress.
    --
    -- -   @available@ - The AFI is available for use.
    --
    -- -   @failed@ - AFI bitstream generation failed.
    --
    -- -   @unavailable@ - The AFI is no longer available for use.
    code :: Prelude.Maybe FpgaImageStateCode,
    -- | If the state is @failed@, this is the error message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FpgaImageState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'message', 'fpgaImageState_message' - If the state is @failed@, this is the error message.
newFpgaImageState ::
  FpgaImageState
newFpgaImageState =
  FpgaImageState'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

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

-- | If the state is @failed@, this is the error message.
fpgaImageState_message :: Lens.Lens' FpgaImageState (Prelude.Maybe Prelude.Text)
fpgaImageState_message = Lens.lens (\FpgaImageState' {message} -> message) (\s@FpgaImageState' {} a -> s {message = a} :: FpgaImageState)

instance Data.FromXML FpgaImageState where
  parseXML x =
    FpgaImageState'
      Prelude.<$> (x Data..@? "code")
      Prelude.<*> (x Data..@? "message")

instance Prelude.Hashable FpgaImageState where
  hashWithSalt _salt FpgaImageState' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData FpgaImageState where
  rnf FpgaImageState' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
