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
-- Module      : Network.AWS.EC2.Types.IamInstanceProfileAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IamInstanceProfileAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.IamInstanceProfile
import Network.AWS.EC2.Types.IamInstanceProfileAssociationState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an association between an IAM instance profile and an
-- instance.
--
-- /See:/ 'newIamInstanceProfileAssociation' smart constructor.
data IamInstanceProfileAssociation = IamInstanceProfileAssociation'
  { -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The IAM instance profile.
    iamInstanceProfile :: Prelude.Maybe IamInstanceProfile,
    -- | The state of the association.
    state :: Prelude.Maybe IamInstanceProfileAssociationState,
    -- | The ID of the association.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The time the IAM instance profile was associated with the instance.
    timestamp :: Prelude.Maybe Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'IamInstanceProfileAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'iamInstanceProfileAssociation_instanceId' - The ID of the instance.
--
-- 'iamInstanceProfile', 'iamInstanceProfileAssociation_iamInstanceProfile' - The IAM instance profile.
--
-- 'state', 'iamInstanceProfileAssociation_state' - The state of the association.
--
-- 'associationId', 'iamInstanceProfileAssociation_associationId' - The ID of the association.
--
-- 'timestamp', 'iamInstanceProfileAssociation_timestamp' - The time the IAM instance profile was associated with the instance.
newIamInstanceProfileAssociation ::
  IamInstanceProfileAssociation
newIamInstanceProfileAssociation =
  IamInstanceProfileAssociation'
    { instanceId =
        Prelude.Nothing,
      iamInstanceProfile = Prelude.Nothing,
      state = Prelude.Nothing,
      associationId = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The ID of the instance.
iamInstanceProfileAssociation_instanceId :: Lens.Lens' IamInstanceProfileAssociation (Prelude.Maybe Prelude.Text)
iamInstanceProfileAssociation_instanceId = Lens.lens (\IamInstanceProfileAssociation' {instanceId} -> instanceId) (\s@IamInstanceProfileAssociation' {} a -> s {instanceId = a} :: IamInstanceProfileAssociation)

-- | The IAM instance profile.
iamInstanceProfileAssociation_iamInstanceProfile :: Lens.Lens' IamInstanceProfileAssociation (Prelude.Maybe IamInstanceProfile)
iamInstanceProfileAssociation_iamInstanceProfile = Lens.lens (\IamInstanceProfileAssociation' {iamInstanceProfile} -> iamInstanceProfile) (\s@IamInstanceProfileAssociation' {} a -> s {iamInstanceProfile = a} :: IamInstanceProfileAssociation)

-- | The state of the association.
iamInstanceProfileAssociation_state :: Lens.Lens' IamInstanceProfileAssociation (Prelude.Maybe IamInstanceProfileAssociationState)
iamInstanceProfileAssociation_state = Lens.lens (\IamInstanceProfileAssociation' {state} -> state) (\s@IamInstanceProfileAssociation' {} a -> s {state = a} :: IamInstanceProfileAssociation)

-- | The ID of the association.
iamInstanceProfileAssociation_associationId :: Lens.Lens' IamInstanceProfileAssociation (Prelude.Maybe Prelude.Text)
iamInstanceProfileAssociation_associationId = Lens.lens (\IamInstanceProfileAssociation' {associationId} -> associationId) (\s@IamInstanceProfileAssociation' {} a -> s {associationId = a} :: IamInstanceProfileAssociation)

-- | The time the IAM instance profile was associated with the instance.
iamInstanceProfileAssociation_timestamp :: Lens.Lens' IamInstanceProfileAssociation (Prelude.Maybe Prelude.UTCTime)
iamInstanceProfileAssociation_timestamp = Lens.lens (\IamInstanceProfileAssociation' {timestamp} -> timestamp) (\s@IamInstanceProfileAssociation' {} a -> s {timestamp = a} :: IamInstanceProfileAssociation) Prelude.. Lens.mapping Prelude._Time

instance
  Prelude.FromXML
    IamInstanceProfileAssociation
  where
  parseXML x =
    IamInstanceProfileAssociation'
      Prelude.<$> (x Prelude..@? "instanceId")
      Prelude.<*> (x Prelude..@? "iamInstanceProfile")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> (x Prelude..@? "associationId")
      Prelude.<*> (x Prelude..@? "timestamp")

instance
  Prelude.Hashable
    IamInstanceProfileAssociation

instance Prelude.NFData IamInstanceProfileAssociation
