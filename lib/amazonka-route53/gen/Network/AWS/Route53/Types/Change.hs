{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.Change
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.Change where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.ChangeAction
import Network.AWS.Route53.Types.ResourceRecordSet

-- | The information for each resource record set that you want to change.
--
--
--
-- /See:/ 'change' smart constructor.
data Change = Change'
  { _cAction :: !ChangeAction,
    _cResourceRecordSet :: !ResourceRecordSet
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Change' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cAction' - The action to perform:     * @CREATE@ : Creates a resource record set that has the specified values.     * @DELETE@ : Deletes a existing resource record set. /Important:/ To delete the resource record set that is associated with a traffic policy instance, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteTrafficPolicyInstance.html DeleteTrafficPolicyInstance> . Amazon Route 53 will delete the resource record set automatically. If you delete the resource record set by using @ChangeResourceRecordSets@ , Route 53 doesn't automatically delete the traffic policy instance, and you'll continue to be charged for it even though it's no longer in use.      * @UPSERT@ : If a resource record set doesn't already exist, Route 53 creates it. If a resource record set does exist, Route 53 updates it with the values in the request.
--
-- * 'cResourceRecordSet' - Information about the resource record set to create, delete, or update.
change ::
  -- | 'cAction'
  ChangeAction ->
  -- | 'cResourceRecordSet'
  ResourceRecordSet ->
  Change
change pAction_ pResourceRecordSet_ =
  Change'
    { _cAction = pAction_,
      _cResourceRecordSet = pResourceRecordSet_
    }

-- | The action to perform:     * @CREATE@ : Creates a resource record set that has the specified values.     * @DELETE@ : Deletes a existing resource record set. /Important:/ To delete the resource record set that is associated with a traffic policy instance, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteTrafficPolicyInstance.html DeleteTrafficPolicyInstance> . Amazon Route 53 will delete the resource record set automatically. If you delete the resource record set by using @ChangeResourceRecordSets@ , Route 53 doesn't automatically delete the traffic policy instance, and you'll continue to be charged for it even though it's no longer in use.      * @UPSERT@ : If a resource record set doesn't already exist, Route 53 creates it. If a resource record set does exist, Route 53 updates it with the values in the request.
cAction :: Lens' Change ChangeAction
cAction = lens _cAction (\s a -> s {_cAction = a})

-- | Information about the resource record set to create, delete, or update.
cResourceRecordSet :: Lens' Change ResourceRecordSet
cResourceRecordSet = lens _cResourceRecordSet (\s a -> s {_cResourceRecordSet = a})

instance Hashable Change

instance NFData Change

instance ToXML Change where
  toXML Change' {..} =
    mconcat
      ["Action" @= _cAction, "ResourceRecordSet" @= _cResourceRecordSet]
