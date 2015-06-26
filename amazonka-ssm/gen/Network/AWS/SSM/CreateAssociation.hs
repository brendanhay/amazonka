{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SSM.CreateAssociation
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Associates the specified configuration document with the specified
-- instance.
--
-- When you associate a configuration document with an instance, the
-- configuration agent on the instance processes the configuration document
-- and configures the instance as specified.
--
-- If you associate a configuration document with an instance that already
-- has an associated configuration document, we replace the current
-- configuration document with the new configuration document.
--
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/API_CreateAssociation.html>
module Network.AWS.SSM.CreateAssociation
    (
    -- * Request
      CreateAssociation
    -- ** Request constructor
    , createAssociation
    -- ** Request lenses
    , caName
    , caInstanceId

    -- * Response
    , CreateAssociationResponse
    -- ** Response constructor
    , createAssociationResponse
    -- ** Response lenses
    , carAssociationDescription
    , carStatusCode
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'createAssociation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'caName'
--
-- * 'caInstanceId'
data CreateAssociation = CreateAssociation'{_caName :: Text, _caInstanceId :: Text} deriving (Eq, Read, Show)

-- | 'CreateAssociation' smart constructor.
createAssociation :: Text -> Text -> CreateAssociation
createAssociation pName pInstanceId = CreateAssociation'{_caName = pName, _caInstanceId = pInstanceId};

-- | The name of the configuration document.
caName :: Lens' CreateAssociation Text
caName = lens _caName (\ s a -> s{_caName = a});

-- | The ID of the instance.
caInstanceId :: Lens' CreateAssociation Text
caInstanceId = lens _caInstanceId (\ s a -> s{_caInstanceId = a});

instance AWSRequest CreateAssociation where
        type Sv CreateAssociation = SSM
        type Rs CreateAssociation = CreateAssociationResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateAssociationResponse' <$>
                   (x .?> "AssociationDescription") <*>
                     (pure (fromEnum s)))

instance ToHeaders CreateAssociation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.CreateAssociation" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateAssociation where
        toJSON CreateAssociation'{..}
          = object
              ["Name" .= _caName, "InstanceId" .= _caInstanceId]

instance ToPath CreateAssociation where
        toPath = const "/"

instance ToQuery CreateAssociation where
        toQuery = const mempty

-- | /See:/ 'createAssociationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'carAssociationDescription'
--
-- * 'carStatusCode'
data CreateAssociationResponse = CreateAssociationResponse'{_carAssociationDescription :: Maybe AssociationDescription, _carStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'CreateAssociationResponse' smart constructor.
createAssociationResponse :: Int -> CreateAssociationResponse
createAssociationResponse pStatusCode = CreateAssociationResponse'{_carAssociationDescription = Nothing, _carStatusCode = pStatusCode};

-- | Information about the association.
carAssociationDescription :: Lens' CreateAssociationResponse (Maybe AssociationDescription)
carAssociationDescription = lens _carAssociationDescription (\ s a -> s{_carAssociationDescription = a});

-- | FIXME: Undocumented member.
carStatusCode :: Lens' CreateAssociationResponse Int
carStatusCode = lens _carStatusCode (\ s a -> s{_carStatusCode = a});
