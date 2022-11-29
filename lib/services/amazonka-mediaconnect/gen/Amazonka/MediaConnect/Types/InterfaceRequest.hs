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
-- Module      : Amazonka.MediaConnect.Types.InterfaceRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.InterfaceRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The VPC interface that you want to designate where the media stream is
-- coming from or going to.
--
-- /See:/ 'newInterfaceRequest' smart constructor.
data InterfaceRequest = InterfaceRequest'
  { -- | The name of the VPC interface.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InterfaceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'interfaceRequest_name' - The name of the VPC interface.
newInterfaceRequest ::
  -- | 'name'
  Prelude.Text ->
  InterfaceRequest
newInterfaceRequest pName_ =
  InterfaceRequest' {name = pName_}

-- | The name of the VPC interface.
interfaceRequest_name :: Lens.Lens' InterfaceRequest Prelude.Text
interfaceRequest_name = Lens.lens (\InterfaceRequest' {name} -> name) (\s@InterfaceRequest' {} a -> s {name = a} :: InterfaceRequest)

instance Prelude.Hashable InterfaceRequest where
  hashWithSalt _salt InterfaceRequest' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData InterfaceRequest where
  rnf InterfaceRequest' {..} = Prelude.rnf name

instance Core.ToJSON InterfaceRequest where
  toJSON InterfaceRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Core..= name)]
      )
