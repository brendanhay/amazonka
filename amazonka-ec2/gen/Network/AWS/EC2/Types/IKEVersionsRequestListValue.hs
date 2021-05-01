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
-- Module      : Network.AWS.EC2.Types.IKEVersionsRequestListValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IKEVersionsRequestListValue where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The IKE version that is permitted for the VPN tunnel.
--
-- /See:/ 'newIKEVersionsRequestListValue' smart constructor.
data IKEVersionsRequestListValue = IKEVersionsRequestListValue'
  { -- | The IKE version.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'IKEVersionsRequestListValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'iKEVersionsRequestListValue_value' - The IKE version.
newIKEVersionsRequestListValue ::
  IKEVersionsRequestListValue
newIKEVersionsRequestListValue =
  IKEVersionsRequestListValue'
    { value =
        Prelude.Nothing
    }

-- | The IKE version.
iKEVersionsRequestListValue_value :: Lens.Lens' IKEVersionsRequestListValue (Prelude.Maybe Prelude.Text)
iKEVersionsRequestListValue_value = Lens.lens (\IKEVersionsRequestListValue' {value} -> value) (\s@IKEVersionsRequestListValue' {} a -> s {value = a} :: IKEVersionsRequestListValue)

instance Prelude.Hashable IKEVersionsRequestListValue

instance Prelude.NFData IKEVersionsRequestListValue

instance Prelude.ToQuery IKEVersionsRequestListValue where
  toQuery IKEVersionsRequestListValue' {..} =
    Prelude.mconcat ["Value" Prelude.=: value]
