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
-- Module      : Network.AWS.APIGateway.DeleteUsagePlanKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a usage plan key and remove the underlying API key from the associated usage plan.
--
--
module Network.AWS.APIGateway.DeleteUsagePlanKey
    (
    -- * Creating a Request
      deleteUsagePlanKey
    , DeleteUsagePlanKey
    -- * Request Lenses
    , dupkUsagePlanId
    , dupkKeyId

    -- * Destructuring the Response
    , deleteUsagePlanKeyResponse
    , DeleteUsagePlanKeyResponse
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The DELETE request to delete a usage plan key and remove the underlying API key from the associated usage plan.
--
--
--
-- /See:/ 'deleteUsagePlanKey' smart constructor.
data DeleteUsagePlanKey = DeleteUsagePlanKey'
  { _dupkUsagePlanId :: !Text
  , _dupkKeyId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUsagePlanKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dupkUsagePlanId' - [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-deleted 'UsagePlanKey' resource representing a plan customer.
--
-- * 'dupkKeyId' - [Required] The Id of the 'UsagePlanKey' resource to be deleted.
deleteUsagePlanKey
    :: Text -- ^ 'dupkUsagePlanId'
    -> Text -- ^ 'dupkKeyId'
    -> DeleteUsagePlanKey
deleteUsagePlanKey pUsagePlanId_ pKeyId_ =
  DeleteUsagePlanKey' {_dupkUsagePlanId = pUsagePlanId_, _dupkKeyId = pKeyId_}


-- | [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-deleted 'UsagePlanKey' resource representing a plan customer.
dupkUsagePlanId :: Lens' DeleteUsagePlanKey Text
dupkUsagePlanId = lens _dupkUsagePlanId (\ s a -> s{_dupkUsagePlanId = a})

-- | [Required] The Id of the 'UsagePlanKey' resource to be deleted.
dupkKeyId :: Lens' DeleteUsagePlanKey Text
dupkKeyId = lens _dupkKeyId (\ s a -> s{_dupkKeyId = a})

instance AWSRequest DeleteUsagePlanKey where
        type Rs DeleteUsagePlanKey =
             DeleteUsagePlanKeyResponse
        request = delete apiGateway
        response = receiveNull DeleteUsagePlanKeyResponse'

instance Hashable DeleteUsagePlanKey where

instance NFData DeleteUsagePlanKey where

instance ToHeaders DeleteUsagePlanKey where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath DeleteUsagePlanKey where
        toPath DeleteUsagePlanKey'{..}
          = mconcat
              ["/usageplans/", toBS _dupkUsagePlanId, "/keys/",
               toBS _dupkKeyId]

instance ToQuery DeleteUsagePlanKey where
        toQuery = const mempty

-- | /See:/ 'deleteUsagePlanKeyResponse' smart constructor.
data DeleteUsagePlanKeyResponse =
  DeleteUsagePlanKeyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUsagePlanKeyResponse' with the minimum fields required to make a request.
--
deleteUsagePlanKeyResponse
    :: DeleteUsagePlanKeyResponse
deleteUsagePlanKeyResponse = DeleteUsagePlanKeyResponse'


instance NFData DeleteUsagePlanKeyResponse where
