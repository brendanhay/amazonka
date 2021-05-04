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
-- Module      : Network.AWS.EC2.Types.DeleteFleetError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteFleetError where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DeleteFleetErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an EC2 Fleet error.
--
-- /See:/ 'newDeleteFleetError' smart constructor.
data DeleteFleetError = DeleteFleetError'
  { -- | The description for the error code.
    message :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    code :: Prelude.Maybe DeleteFleetErrorCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleetError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'deleteFleetError_message' - The description for the error code.
--
-- 'code', 'deleteFleetError_code' - The error code.
newDeleteFleetError ::
  DeleteFleetError
newDeleteFleetError =
  DeleteFleetError'
    { message = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The description for the error code.
deleteFleetError_message :: Lens.Lens' DeleteFleetError (Prelude.Maybe Prelude.Text)
deleteFleetError_message = Lens.lens (\DeleteFleetError' {message} -> message) (\s@DeleteFleetError' {} a -> s {message = a} :: DeleteFleetError)

-- | The error code.
deleteFleetError_code :: Lens.Lens' DeleteFleetError (Prelude.Maybe DeleteFleetErrorCode)
deleteFleetError_code = Lens.lens (\DeleteFleetError' {code} -> code) (\s@DeleteFleetError' {} a -> s {code = a} :: DeleteFleetError)

instance Prelude.FromXML DeleteFleetError where
  parseXML x =
    DeleteFleetError'
      Prelude.<$> (x Prelude..@? "message")
      Prelude.<*> (x Prelude..@? "code")

instance Prelude.Hashable DeleteFleetError

instance Prelude.NFData DeleteFleetError
