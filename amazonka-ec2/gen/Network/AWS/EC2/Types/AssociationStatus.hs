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
-- Module      : Network.AWS.EC2.Types.AssociationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AssociationStatus where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AssociationStatusCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the state of a target network association.
--
-- /See:/ 'newAssociationStatus' smart constructor.
data AssociationStatus = AssociationStatus'
  { -- | A message about the status of the target network association, if
    -- applicable.
    message :: Prelude.Maybe Prelude.Text,
    -- | The state of the target network association.
    code :: Prelude.Maybe AssociationStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'associationStatus_message' - A message about the status of the target network association, if
-- applicable.
--
-- 'code', 'associationStatus_code' - The state of the target network association.
newAssociationStatus ::
  AssociationStatus
newAssociationStatus =
  AssociationStatus'
    { message = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | A message about the status of the target network association, if
-- applicable.
associationStatus_message :: Lens.Lens' AssociationStatus (Prelude.Maybe Prelude.Text)
associationStatus_message = Lens.lens (\AssociationStatus' {message} -> message) (\s@AssociationStatus' {} a -> s {message = a} :: AssociationStatus)

-- | The state of the target network association.
associationStatus_code :: Lens.Lens' AssociationStatus (Prelude.Maybe AssociationStatusCode)
associationStatus_code = Lens.lens (\AssociationStatus' {code} -> code) (\s@AssociationStatus' {} a -> s {code = a} :: AssociationStatus)

instance Prelude.FromXML AssociationStatus where
  parseXML x =
    AssociationStatus'
      Prelude.<$> (x Prelude..@? "message")
      Prelude.<*> (x Prelude..@? "code")

instance Prelude.Hashable AssociationStatus

instance Prelude.NFData AssociationStatus
