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
-- Module      : Network.AWS.Route53.Types.Change
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.Change where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.ChangeAction
import Network.AWS.Route53.Types.ResourceRecordSet

-- | The information for each resource record set that you want to change.
--
-- /See:/ 'newChange' smart constructor.
data Change = Change'
  { -- | The action to perform:
    --
    -- -   @CREATE@: Creates a resource record set that has the specified
    --     values.
    --
    -- -   @DELETE@: Deletes a existing resource record set.
    --
    --     To delete the resource record set that is associated with a traffic
    --     policy instance, use
    --     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteTrafficPolicyInstance.html DeleteTrafficPolicyInstance>.
    --     Amazon Route 53 will delete the resource record set automatically.
    --     If you delete the resource record set by using
    --     @ChangeResourceRecordSets@, Route 53 doesn\'t automatically delete
    --     the traffic policy instance, and you\'ll continue to be charged for
    --     it even though it\'s no longer in use.
    --
    -- -   @UPSERT@: If a resource record set doesn\'t already exist, Route 53
    --     creates it. If a resource record set does exist, Route 53 updates it
    --     with the values in the request.
    action :: ChangeAction,
    -- | Information about the resource record set to create, delete, or update.
    resourceRecordSet :: ResourceRecordSet
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Change' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'change_action' - The action to perform:
--
-- -   @CREATE@: Creates a resource record set that has the specified
--     values.
--
-- -   @DELETE@: Deletes a existing resource record set.
--
--     To delete the resource record set that is associated with a traffic
--     policy instance, use
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteTrafficPolicyInstance.html DeleteTrafficPolicyInstance>.
--     Amazon Route 53 will delete the resource record set automatically.
--     If you delete the resource record set by using
--     @ChangeResourceRecordSets@, Route 53 doesn\'t automatically delete
--     the traffic policy instance, and you\'ll continue to be charged for
--     it even though it\'s no longer in use.
--
-- -   @UPSERT@: If a resource record set doesn\'t already exist, Route 53
--     creates it. If a resource record set does exist, Route 53 updates it
--     with the values in the request.
--
-- 'resourceRecordSet', 'change_resourceRecordSet' - Information about the resource record set to create, delete, or update.
newChange ::
  -- | 'action'
  ChangeAction ->
  -- | 'resourceRecordSet'
  ResourceRecordSet ->
  Change
newChange pAction_ pResourceRecordSet_ =
  Change'
    { action = pAction_,
      resourceRecordSet = pResourceRecordSet_
    }

-- | The action to perform:
--
-- -   @CREATE@: Creates a resource record set that has the specified
--     values.
--
-- -   @DELETE@: Deletes a existing resource record set.
--
--     To delete the resource record set that is associated with a traffic
--     policy instance, use
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteTrafficPolicyInstance.html DeleteTrafficPolicyInstance>.
--     Amazon Route 53 will delete the resource record set automatically.
--     If you delete the resource record set by using
--     @ChangeResourceRecordSets@, Route 53 doesn\'t automatically delete
--     the traffic policy instance, and you\'ll continue to be charged for
--     it even though it\'s no longer in use.
--
-- -   @UPSERT@: If a resource record set doesn\'t already exist, Route 53
--     creates it. If a resource record set does exist, Route 53 updates it
--     with the values in the request.
change_action :: Lens.Lens' Change ChangeAction
change_action = Lens.lens (\Change' {action} -> action) (\s@Change' {} a -> s {action = a} :: Change)

-- | Information about the resource record set to create, delete, or update.
change_resourceRecordSet :: Lens.Lens' Change ResourceRecordSet
change_resourceRecordSet = Lens.lens (\Change' {resourceRecordSet} -> resourceRecordSet) (\s@Change' {} a -> s {resourceRecordSet = a} :: Change)

instance Prelude.Hashable Change

instance Prelude.NFData Change

instance Prelude.ToXML Change where
  toXML Change' {..} =
    Prelude.mconcat
      [ "Action" Prelude.@= action,
        "ResourceRecordSet" Prelude.@= resourceRecordSet
      ]
