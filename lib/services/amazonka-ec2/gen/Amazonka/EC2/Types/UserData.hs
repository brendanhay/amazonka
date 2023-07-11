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
-- Module      : Amazonka.EC2.Types.UserData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.UserData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the user data for an instance.
--
-- /See:/ 'newUserData' smart constructor.
data UserData = UserData'
  { -- | The user data. If you are using an Amazon Web Services SDK or command
    -- line tool, Base64-encoding is performed for you, and you can load the
    -- text from a file. Otherwise, you must provide Base64-encoded text.
    data' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'userData_data' - The user data. If you are using an Amazon Web Services SDK or command
-- line tool, Base64-encoding is performed for you, and you can load the
-- text from a file. Otherwise, you must provide Base64-encoded text.
newUserData ::
  UserData
newUserData = UserData' {data' = Prelude.Nothing}

-- | The user data. If you are using an Amazon Web Services SDK or command
-- line tool, Base64-encoding is performed for you, and you can load the
-- text from a file. Otherwise, you must provide Base64-encoded text.
userData_data :: Lens.Lens' UserData (Prelude.Maybe Prelude.Text)
userData_data = Lens.lens (\UserData' {data'} -> data') (\s@UserData' {} a -> s {data' = a} :: UserData)

instance Prelude.Hashable UserData where
  hashWithSalt _salt UserData' {..} =
    _salt `Prelude.hashWithSalt` data'

instance Prelude.NFData UserData where
  rnf UserData' {..} = Prelude.rnf data'

instance Data.ToQuery UserData where
  toQuery UserData' {..} =
    Prelude.mconcat ["Data" Data.=: data']
