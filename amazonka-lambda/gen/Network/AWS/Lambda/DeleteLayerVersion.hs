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
-- Module      : Network.AWS.Lambda.DeleteLayerVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a version of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> . Deleted versions can no longer be viewed or added to functions. To avoid breaking functions, a copy of the version remains in Lambda until no functions refer to it.
--
--
module Network.AWS.Lambda.DeleteLayerVersion
    (
    -- * Creating a Request
      deleteLayerVersion
    , DeleteLayerVersion
    -- * Request Lenses
    , dlvLayerName
    , dlvVersionNumber

    -- * Destructuring the Response
    , deleteLayerVersionResponse
    , DeleteLayerVersionResponse
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLayerVersion' smart constructor.
data DeleteLayerVersion = DeleteLayerVersion'
  { _dlvLayerName     :: !Text
  , _dlvVersionNumber :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLayerVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlvLayerName' - The name or Amazon Resource Name (ARN) of the layer.
--
-- * 'dlvVersionNumber' - The version number.
deleteLayerVersion
    :: Text -- ^ 'dlvLayerName'
    -> Integer -- ^ 'dlvVersionNumber'
    -> DeleteLayerVersion
deleteLayerVersion pLayerName_ pVersionNumber_ =
  DeleteLayerVersion'
    {_dlvLayerName = pLayerName_, _dlvVersionNumber = pVersionNumber_}


-- | The name or Amazon Resource Name (ARN) of the layer.
dlvLayerName :: Lens' DeleteLayerVersion Text
dlvLayerName = lens _dlvLayerName (\ s a -> s{_dlvLayerName = a})

-- | The version number.
dlvVersionNumber :: Lens' DeleteLayerVersion Integer
dlvVersionNumber = lens _dlvVersionNumber (\ s a -> s{_dlvVersionNumber = a})

instance AWSRequest DeleteLayerVersion where
        type Rs DeleteLayerVersion =
             DeleteLayerVersionResponse
        request = delete lambda
        response = receiveNull DeleteLayerVersionResponse'

instance Hashable DeleteLayerVersion where

instance NFData DeleteLayerVersion where

instance ToHeaders DeleteLayerVersion where
        toHeaders = const mempty

instance ToPath DeleteLayerVersion where
        toPath DeleteLayerVersion'{..}
          = mconcat
              ["/2018-10-31/layers/", toBS _dlvLayerName,
               "/versions/", toBS _dlvVersionNumber]

instance ToQuery DeleteLayerVersion where
        toQuery = const mempty

-- | /See:/ 'deleteLayerVersionResponse' smart constructor.
data DeleteLayerVersionResponse =
  DeleteLayerVersionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLayerVersionResponse' with the minimum fields required to make a request.
--
deleteLayerVersionResponse
    :: DeleteLayerVersionResponse
deleteLayerVersionResponse = DeleteLayerVersionResponse'


instance NFData DeleteLayerVersionResponse where
