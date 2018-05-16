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
-- Module      : Network.AWS.APIGateway.DeleteModel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a model.
--
--
module Network.AWS.APIGateway.DeleteModel
    (
    -- * Creating a Request
      deleteModel
    , DeleteModel
    -- * Request Lenses
    , dRestAPIId
    , dModelName

    -- * Destructuring the Response
    , deleteModelResponse
    , DeleteModelResponse
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to delete an existing model in an existing 'RestApi' resource.
--
--
--
-- /See:/ 'deleteModel' smart constructor.
data DeleteModel = DeleteModel'
  { _dRestAPIId :: !Text
  , _dModelName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'dModelName' - [Required] The name of the model to delete.
deleteModel
    :: Text -- ^ 'dRestAPIId'
    -> Text -- ^ 'dModelName'
    -> DeleteModel
deleteModel pRestAPIId_ pModelName_ =
  DeleteModel' {_dRestAPIId = pRestAPIId_, _dModelName = pModelName_}


-- | [Required] The string identifier of the associated 'RestApi' .
dRestAPIId :: Lens' DeleteModel Text
dRestAPIId = lens _dRestAPIId (\ s a -> s{_dRestAPIId = a})

-- | [Required] The name of the model to delete.
dModelName :: Lens' DeleteModel Text
dModelName = lens _dModelName (\ s a -> s{_dModelName = a})

instance AWSRequest DeleteModel where
        type Rs DeleteModel = DeleteModelResponse
        request = delete apiGateway
        response = receiveNull DeleteModelResponse'

instance Hashable DeleteModel where

instance NFData DeleteModel where

instance ToHeaders DeleteModel where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath DeleteModel where
        toPath DeleteModel'{..}
          = mconcat
              ["/restapis/", toBS _dRestAPIId, "/models/",
               toBS _dModelName]

instance ToQuery DeleteModel where
        toQuery = const mempty

-- | /See:/ 'deleteModelResponse' smart constructor.
data DeleteModelResponse =
  DeleteModelResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteModelResponse' with the minimum fields required to make a request.
--
deleteModelResponse
    :: DeleteModelResponse
deleteModelResponse = DeleteModelResponse'


instance NFData DeleteModelResponse where
