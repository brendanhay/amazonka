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
-- Module      : Network.AWS.SSM.CreateAssociation
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified SSM document with the specified instance.
--
-- When you associate an SSM document with an instance, the configuration agent on the instance processes the document and configures the instance as specified.
--
-- If you associate a document with an instance that already has an associated document, the system throws the AssociationAlreadyExists exception.
module Network.AWS.SSM.CreateAssociation
    (
    -- * Creating a Request
      createAssociation
    , CreateAssociation
    -- * Request Lenses
    , caParameters
    , caName
    , caInstanceId

    -- * Destructuring the Response
    , createAssociationResponse
    , CreateAssociationResponse
    -- * Response Lenses
    , crsAssociationDescription
    , crsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types
import           Network.AWS.SSM.Types.Product

-- | /See:/ 'createAssociation' smart constructor.
data CreateAssociation = CreateAssociation'
    { _caParameters :: !(Maybe (Map Text [Text]))
    , _caName       :: !Text
    , _caInstanceId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caParameters'
--
-- * 'caName'
--
-- * 'caInstanceId'
createAssociation
    :: Text -- ^ 'caName'
    -> Text -- ^ 'caInstanceId'
    -> CreateAssociation
createAssociation pName_ pInstanceId_ =
    CreateAssociation'
    { _caParameters = Nothing
    , _caName = pName_
    , _caInstanceId = pInstanceId_
    }

-- | The parameters for the documents runtime configuration.
caParameters :: Lens' CreateAssociation (HashMap Text [Text])
caParameters = lens _caParameters (\ s a -> s{_caParameters = a}) . _Default . _Map;

-- | The name of the SSM document.
caName :: Lens' CreateAssociation Text
caName = lens _caName (\ s a -> s{_caName = a});

-- | The instance ID.
caInstanceId :: Lens' CreateAssociation Text
caInstanceId = lens _caInstanceId (\ s a -> s{_caInstanceId = a});

instance AWSRequest CreateAssociation where
        type Rs CreateAssociation = CreateAssociationResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 CreateAssociationResponse' <$>
                   (x .?> "AssociationDescription") <*>
                     (pure (fromEnum s)))

instance Hashable CreateAssociation

instance NFData CreateAssociation

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
              (catMaybes
                 [("Parameters" .=) <$> _caParameters,
                  Just ("Name" .= _caName),
                  Just ("InstanceId" .= _caInstanceId)])

instance ToPath CreateAssociation where
        toPath = const "/"

instance ToQuery CreateAssociation where
        toQuery = const mempty

-- | /See:/ 'createAssociationResponse' smart constructor.
data CreateAssociationResponse = CreateAssociationResponse'
    { _crsAssociationDescription :: !(Maybe AssociationDescription)
    , _crsResponseStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsAssociationDescription'
--
-- * 'crsResponseStatus'
createAssociationResponse
    :: Int -- ^ 'crsResponseStatus'
    -> CreateAssociationResponse
createAssociationResponse pResponseStatus_ =
    CreateAssociationResponse'
    { _crsAssociationDescription = Nothing
    , _crsResponseStatus = pResponseStatus_
    }

-- | Information about the association.
crsAssociationDescription :: Lens' CreateAssociationResponse (Maybe AssociationDescription)
crsAssociationDescription = lens _crsAssociationDescription (\ s a -> s{_crsAssociationDescription = a});

-- | The response status code.
crsResponseStatus :: Lens' CreateAssociationResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a});

instance NFData CreateAssociationResponse
