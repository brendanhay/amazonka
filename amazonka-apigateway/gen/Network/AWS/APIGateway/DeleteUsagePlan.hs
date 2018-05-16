{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteUsagePlan
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a usage plan of a given plan Id.
--
--
module Network.AWS.APIGateway.DeleteUsagePlan
    (
    -- * Creating a Request
      deleteUsagePlan
    , DeleteUsagePlan
    -- * Request Lenses
    , dupUsagePlanId

    -- * Destructuring the Response
    , deleteUsagePlanResponse
    , DeleteUsagePlanResponse
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The DELETE request to delete a usage plan of a given plan Id.
--
--
--
-- /See:/ 'deleteUsagePlan' smart constructor.
newtype DeleteUsagePlan = DeleteUsagePlan'
  { _dupUsagePlanId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUsagePlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dupUsagePlanId' - [Required] The Id of the to-be-deleted usage plan.
deleteUsagePlan
    :: Text -- ^ 'dupUsagePlanId'
    -> DeleteUsagePlan
deleteUsagePlan pUsagePlanId_ =
  DeleteUsagePlan' {_dupUsagePlanId = pUsagePlanId_}


-- | [Required] The Id of the to-be-deleted usage plan.
dupUsagePlanId :: Lens' DeleteUsagePlan Text
dupUsagePlanId = lens _dupUsagePlanId (\ s a -> s{_dupUsagePlanId = a})

instance AWSRequest DeleteUsagePlan where
        type Rs DeleteUsagePlan = DeleteUsagePlanResponse
        request = delete apiGateway
        response = receiveNull DeleteUsagePlanResponse'

instance Hashable DeleteUsagePlan where

instance NFData DeleteUsagePlan where

instance ToHeaders DeleteUsagePlan where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath DeleteUsagePlan where
        toPath DeleteUsagePlan'{..}
          = mconcat ["/usageplans/", toBS _dupUsagePlanId]

instance ToQuery DeleteUsagePlan where
        toQuery = const mempty

-- | /See:/ 'deleteUsagePlanResponse' smart constructor.
data DeleteUsagePlanResponse =
  DeleteUsagePlanResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUsagePlanResponse' with the minimum fields required to make a request.
--
deleteUsagePlanResponse
    :: DeleteUsagePlanResponse
deleteUsagePlanResponse = DeleteUsagePlanResponse'


instance NFData DeleteUsagePlanResponse where
