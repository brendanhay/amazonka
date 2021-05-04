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
-- Module      : Network.AWS.EC2.Types.IKEVersionsListValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IKEVersionsListValue where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The internet key exchange (IKE) version permitted for the VPN tunnel.
--
-- /See:/ 'newIKEVersionsListValue' smart constructor.
data IKEVersionsListValue = IKEVersionsListValue'
  { -- | The IKE version.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'IKEVersionsListValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'iKEVersionsListValue_value' - The IKE version.
newIKEVersionsListValue ::
  IKEVersionsListValue
newIKEVersionsListValue =
  IKEVersionsListValue' {value = Prelude.Nothing}

-- | The IKE version.
iKEVersionsListValue_value :: Lens.Lens' IKEVersionsListValue (Prelude.Maybe Prelude.Text)
iKEVersionsListValue_value = Lens.lens (\IKEVersionsListValue' {value} -> value) (\s@IKEVersionsListValue' {} a -> s {value = a} :: IKEVersionsListValue)

instance Prelude.FromXML IKEVersionsListValue where
  parseXML x =
    IKEVersionsListValue'
      Prelude.<$> (x Prelude..@? "value")

instance Prelude.Hashable IKEVersionsListValue

instance Prelude.NFData IKEVersionsListValue
