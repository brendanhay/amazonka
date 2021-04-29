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
-- Module      : Network.AWS.EMR.Types.InstanceStateChangeReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceStateChangeReason where

import Network.AWS.EMR.Types.InstanceStateChangeReasonCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The details of the status change reason for the instance.
--
-- /See:/ 'newInstanceStateChangeReason' smart constructor.
data InstanceStateChangeReason = InstanceStateChangeReason'
  { -- | The status change reason description.
    message :: Prelude.Maybe Prelude.Text,
    -- | The programmable code for the state change reason.
    code :: Prelude.Maybe InstanceStateChangeReasonCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstanceStateChangeReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'instanceStateChangeReason_message' - The status change reason description.
--
-- 'code', 'instanceStateChangeReason_code' - The programmable code for the state change reason.
newInstanceStateChangeReason ::
  InstanceStateChangeReason
newInstanceStateChangeReason =
  InstanceStateChangeReason'
    { message =
        Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The status change reason description.
instanceStateChangeReason_message :: Lens.Lens' InstanceStateChangeReason (Prelude.Maybe Prelude.Text)
instanceStateChangeReason_message = Lens.lens (\InstanceStateChangeReason' {message} -> message) (\s@InstanceStateChangeReason' {} a -> s {message = a} :: InstanceStateChangeReason)

-- | The programmable code for the state change reason.
instanceStateChangeReason_code :: Lens.Lens' InstanceStateChangeReason (Prelude.Maybe InstanceStateChangeReasonCode)
instanceStateChangeReason_code = Lens.lens (\InstanceStateChangeReason' {code} -> code) (\s@InstanceStateChangeReason' {} a -> s {code = a} :: InstanceStateChangeReason)

instance Prelude.FromJSON InstanceStateChangeReason where
  parseJSON =
    Prelude.withObject
      "InstanceStateChangeReason"
      ( \x ->
          InstanceStateChangeReason'
            Prelude.<$> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "Code")
      )

instance Prelude.Hashable InstanceStateChangeReason

instance Prelude.NFData InstanceStateChangeReason
