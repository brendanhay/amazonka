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
-- Module      : Network.AWS.APIGateway.UpdateUsagePlan
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a usage plan of a given plan Id.
--
--
module Network.AWS.APIGateway.UpdateUsagePlan
    (
    -- * Creating a Request
      updateUsagePlan
    , UpdateUsagePlan
    -- * Request Lenses
    , uupPatchOperations
    , uupUsagePlanId

    -- * Destructuring the Response
    , usagePlan
    , UsagePlan
    -- * Response Lenses
    , upApiStages
    , upName
    , upId
    , upThrottle
    , upQuota
    , upDescription
    , upProductCode
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The PATCH request to update a usage plan of a given plan Id.
--
--
--
-- /See:/ 'updateUsagePlan' smart constructor.
data UpdateUsagePlan = UpdateUsagePlan'
  { _uupPatchOperations :: !(Maybe [PatchOperation])
  , _uupUsagePlanId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUsagePlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uupPatchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- * 'uupUsagePlanId' - [Required] The Id of the to-be-updated usage plan.
updateUsagePlan
    :: Text -- ^ 'uupUsagePlanId'
    -> UpdateUsagePlan
updateUsagePlan pUsagePlanId_ =
  UpdateUsagePlan'
    {_uupPatchOperations = Nothing, _uupUsagePlanId = pUsagePlanId_}


-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
uupPatchOperations :: Lens' UpdateUsagePlan [PatchOperation]
uupPatchOperations = lens _uupPatchOperations (\ s a -> s{_uupPatchOperations = a}) . _Default . _Coerce

-- | [Required] The Id of the to-be-updated usage plan.
uupUsagePlanId :: Lens' UpdateUsagePlan Text
uupUsagePlanId = lens _uupUsagePlanId (\ s a -> s{_uupUsagePlanId = a})

instance AWSRequest UpdateUsagePlan where
        type Rs UpdateUsagePlan = UsagePlan
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateUsagePlan where

instance NFData UpdateUsagePlan where

instance ToHeaders UpdateUsagePlan where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateUsagePlan where
        toJSON UpdateUsagePlan'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$> _uupPatchOperations])

instance ToPath UpdateUsagePlan where
        toPath UpdateUsagePlan'{..}
          = mconcat ["/usageplans/", toBS _uupUsagePlanId]

instance ToQuery UpdateUsagePlan where
        toQuery = const mempty
