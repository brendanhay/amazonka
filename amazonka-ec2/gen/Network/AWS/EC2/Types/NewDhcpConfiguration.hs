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
-- Module      : Network.AWS.EC2.Types.NewDhcpConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NewDhcpConfiguration where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newNewDhcpConfiguration' smart constructor.
data NewDhcpConfiguration = NewDhcpConfiguration'
  { key :: Prelude.Maybe Prelude.Text,
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NewDhcpConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'newDhcpConfiguration_key' - Undocumented member.
--
-- 'values', 'newDhcpConfiguration_values' - Undocumented member.
newNewDhcpConfiguration ::
  NewDhcpConfiguration
newNewDhcpConfiguration =
  NewDhcpConfiguration'
    { key = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | Undocumented member.
newDhcpConfiguration_key :: Lens.Lens' NewDhcpConfiguration (Prelude.Maybe Prelude.Text)
newDhcpConfiguration_key = Lens.lens (\NewDhcpConfiguration' {key} -> key) (\s@NewDhcpConfiguration' {} a -> s {key = a} :: NewDhcpConfiguration)

-- | Undocumented member.
newDhcpConfiguration_values :: Lens.Lens' NewDhcpConfiguration (Prelude.Maybe [Prelude.Text])
newDhcpConfiguration_values = Lens.lens (\NewDhcpConfiguration' {values} -> values) (\s@NewDhcpConfiguration' {} a -> s {values = a} :: NewDhcpConfiguration) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.Hashable NewDhcpConfiguration

instance Prelude.NFData NewDhcpConfiguration

instance Prelude.ToQuery NewDhcpConfiguration where
  toQuery NewDhcpConfiguration' {..} =
    Prelude.mconcat
      [ "Key" Prelude.=: key,
        Prelude.toQuery
          (Prelude.toQueryList "Value" Prelude.<$> values)
      ]
