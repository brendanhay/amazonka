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
-- Module      : Network.AWS.APIGateway.UpdateUsage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants a temporary extension to the remaining quota of a usage plan associated with a specified API key.
--
--
module Network.AWS.APIGateway.UpdateUsage
    (
    -- * Creating a Request
      updateUsage
    , UpdateUsage
    -- * Request Lenses
    , uuPatchOperations
    , uuUsagePlanId
    , uuKeyId

    -- * Destructuring the Response
    , usage
    , Usage
    -- * Response Lenses
    , uUsagePlanId
    , uEndDate
    , uItems
    , uStartDate
    , uPosition
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The PATCH request to grant a temporary extension to the remaining quota of a usage plan associated with a specified API key.
--
--
--
-- /See:/ 'updateUsage' smart constructor.
data UpdateUsage = UpdateUsage'
  { _uuPatchOperations :: !(Maybe [PatchOperation])
  , _uuUsagePlanId     :: !Text
  , _uuKeyId           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUsage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uuPatchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- * 'uuUsagePlanId' - [Required] The Id of the usage plan associated with the usage data.
--
-- * 'uuKeyId' - [Required] The identifier of the API key associated with the usage plan in which a temporary extension is granted to the remaining quota.
updateUsage
    :: Text -- ^ 'uuUsagePlanId'
    -> Text -- ^ 'uuKeyId'
    -> UpdateUsage
updateUsage pUsagePlanId_ pKeyId_ =
  UpdateUsage'
    { _uuPatchOperations = Nothing
    , _uuUsagePlanId = pUsagePlanId_
    , _uuKeyId = pKeyId_
    }


-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
uuPatchOperations :: Lens' UpdateUsage [PatchOperation]
uuPatchOperations = lens _uuPatchOperations (\ s a -> s{_uuPatchOperations = a}) . _Default . _Coerce

-- | [Required] The Id of the usage plan associated with the usage data.
uuUsagePlanId :: Lens' UpdateUsage Text
uuUsagePlanId = lens _uuUsagePlanId (\ s a -> s{_uuUsagePlanId = a})

-- | [Required] The identifier of the API key associated with the usage plan in which a temporary extension is granted to the remaining quota.
uuKeyId :: Lens' UpdateUsage Text
uuKeyId = lens _uuKeyId (\ s a -> s{_uuKeyId = a})

instance AWSRequest UpdateUsage where
        type Rs UpdateUsage = Usage
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateUsage where

instance NFData UpdateUsage where

instance ToHeaders UpdateUsage where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateUsage where
        toJSON UpdateUsage'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$> _uuPatchOperations])

instance ToPath UpdateUsage where
        toPath UpdateUsage'{..}
          = mconcat
              ["/usageplans/", toBS _uuUsagePlanId, "/keys/",
               toBS _uuKeyId, "/usage"]

instance ToQuery UpdateUsage where
        toQuery = const mempty
