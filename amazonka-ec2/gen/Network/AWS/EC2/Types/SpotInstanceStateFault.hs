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
-- Module      : Network.AWS.EC2.Types.SpotInstanceStateFault
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotInstanceStateFault where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a Spot Instance state change.
--
-- /See:/ 'newSpotInstanceStateFault' smart constructor.
data SpotInstanceStateFault = SpotInstanceStateFault'
  { -- | The message for the Spot Instance state change.
    message :: Prelude.Maybe Prelude.Text,
    -- | The reason code for the Spot Instance state change.
    code :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SpotInstanceStateFault' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'spotInstanceStateFault_message' - The message for the Spot Instance state change.
--
-- 'code', 'spotInstanceStateFault_code' - The reason code for the Spot Instance state change.
newSpotInstanceStateFault ::
  SpotInstanceStateFault
newSpotInstanceStateFault =
  SpotInstanceStateFault'
    { message = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The message for the Spot Instance state change.
spotInstanceStateFault_message :: Lens.Lens' SpotInstanceStateFault (Prelude.Maybe Prelude.Text)
spotInstanceStateFault_message = Lens.lens (\SpotInstanceStateFault' {message} -> message) (\s@SpotInstanceStateFault' {} a -> s {message = a} :: SpotInstanceStateFault)

-- | The reason code for the Spot Instance state change.
spotInstanceStateFault_code :: Lens.Lens' SpotInstanceStateFault (Prelude.Maybe Prelude.Text)
spotInstanceStateFault_code = Lens.lens (\SpotInstanceStateFault' {code} -> code) (\s@SpotInstanceStateFault' {} a -> s {code = a} :: SpotInstanceStateFault)

instance Prelude.FromXML SpotInstanceStateFault where
  parseXML x =
    SpotInstanceStateFault'
      Prelude.<$> (x Prelude..@? "message")
      Prelude.<*> (x Prelude..@? "code")

instance Prelude.Hashable SpotInstanceStateFault

instance Prelude.NFData SpotInstanceStateFault
