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
-- Module      : Network.AWS.APIGateway.DeleteRequestValidator
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a 'RequestValidator' of a given 'RestApi' .
--
--
module Network.AWS.APIGateway.DeleteRequestValidator
    (
    -- * Creating a Request
      deleteRequestValidator
    , DeleteRequestValidator
    -- * Request Lenses
    , drvRestAPIId
    , drvRequestValidatorId

    -- * Destructuring the Response
    , deleteRequestValidatorResponse
    , DeleteRequestValidatorResponse
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Deletes a specified 'RequestValidator' of a given 'RestApi' .
--
--
--
-- /See:/ 'deleteRequestValidator' smart constructor.
data DeleteRequestValidator = DeleteRequestValidator'
  { _drvRestAPIId          :: !Text
  , _drvRequestValidatorId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRequestValidator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drvRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'drvRequestValidatorId' - [Required] The identifier of the 'RequestValidator' to be deleted.
deleteRequestValidator
    :: Text -- ^ 'drvRestAPIId'
    -> Text -- ^ 'drvRequestValidatorId'
    -> DeleteRequestValidator
deleteRequestValidator pRestAPIId_ pRequestValidatorId_ =
  DeleteRequestValidator'
    {_drvRestAPIId = pRestAPIId_, _drvRequestValidatorId = pRequestValidatorId_}


-- | [Required] The string identifier of the associated 'RestApi' .
drvRestAPIId :: Lens' DeleteRequestValidator Text
drvRestAPIId = lens _drvRestAPIId (\ s a -> s{_drvRestAPIId = a})

-- | [Required] The identifier of the 'RequestValidator' to be deleted.
drvRequestValidatorId :: Lens' DeleteRequestValidator Text
drvRequestValidatorId = lens _drvRequestValidatorId (\ s a -> s{_drvRequestValidatorId = a})

instance AWSRequest DeleteRequestValidator where
        type Rs DeleteRequestValidator =
             DeleteRequestValidatorResponse
        request = delete apiGateway
        response
          = receiveNull DeleteRequestValidatorResponse'

instance Hashable DeleteRequestValidator where

instance NFData DeleteRequestValidator where

instance ToHeaders DeleteRequestValidator where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath DeleteRequestValidator where
        toPath DeleteRequestValidator'{..}
          = mconcat
              ["/restapis/", toBS _drvRestAPIId,
               "/requestvalidators/", toBS _drvRequestValidatorId]

instance ToQuery DeleteRequestValidator where
        toQuery = const mempty

-- | /See:/ 'deleteRequestValidatorResponse' smart constructor.
data DeleteRequestValidatorResponse =
  DeleteRequestValidatorResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRequestValidatorResponse' with the minimum fields required to make a request.
--
deleteRequestValidatorResponse
    :: DeleteRequestValidatorResponse
deleteRequestValidatorResponse = DeleteRequestValidatorResponse'


instance NFData DeleteRequestValidatorResponse where
