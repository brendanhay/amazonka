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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified Systems Manager document with the specified instances or targets.
--
--
-- When you associate a document with one or more instances using instance IDs or tags, the SSM Agent running on the instance processes the document and configures the instance as specified.
--
-- If you associate a document with an instance that already has an associated document, the system throws the AssociationAlreadyExists exception.
--
module Network.AWS.SSM.CreateAssociation
    (
    -- * Creating a Request
      createAssociation
    , CreateAssociation
    -- * Request Lenses
    , caInstanceId
    , caScheduleExpression
    , caOutputLocation
    , caTargets
    , caParameters
    , caDocumentVersion
    , caAssociationName
    , caName

    -- * Destructuring the Response
    , createAssociationResponse
    , CreateAssociationResponse
    -- * Response Lenses
    , crsAssociationDescription
    , crsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'createAssociation' smart constructor.
data CreateAssociation = CreateAssociation'
  { _caInstanceId         :: !(Maybe Text)
  , _caScheduleExpression :: !(Maybe Text)
  , _caOutputLocation     :: !(Maybe InstanceAssociationOutputLocation)
  , _caTargets            :: !(Maybe [Target])
  , _caParameters         :: !(Maybe (Map Text [Text]))
  , _caDocumentVersion    :: !(Maybe Text)
  , _caAssociationName    :: !(Maybe Text)
  , _caName               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caInstanceId' - The instance ID.
--
-- * 'caScheduleExpression' - A cron expression when the association will be applied to the target(s).
--
-- * 'caOutputLocation' - An Amazon S3 bucket where you want to store the output details of the request.
--
-- * 'caTargets' - The targets (either instances or tags) for the association.
--
-- * 'caParameters' - The parameters for the documents runtime configuration.
--
-- * 'caDocumentVersion' - The document version you want to associate with the target(s). Can be a specific version or the default version.
--
-- * 'caAssociationName' - Specify a descriptive name for the association.
--
-- * 'caName' - The name of the Systems Manager document.
createAssociation
    :: Text -- ^ 'caName'
    -> CreateAssociation
createAssociation pName_ =
  CreateAssociation'
    { _caInstanceId = Nothing
    , _caScheduleExpression = Nothing
    , _caOutputLocation = Nothing
    , _caTargets = Nothing
    , _caParameters = Nothing
    , _caDocumentVersion = Nothing
    , _caAssociationName = Nothing
    , _caName = pName_
    }


-- | The instance ID.
caInstanceId :: Lens' CreateAssociation (Maybe Text)
caInstanceId = lens _caInstanceId (\ s a -> s{_caInstanceId = a})

-- | A cron expression when the association will be applied to the target(s).
caScheduleExpression :: Lens' CreateAssociation (Maybe Text)
caScheduleExpression = lens _caScheduleExpression (\ s a -> s{_caScheduleExpression = a})

-- | An Amazon S3 bucket where you want to store the output details of the request.
caOutputLocation :: Lens' CreateAssociation (Maybe InstanceAssociationOutputLocation)
caOutputLocation = lens _caOutputLocation (\ s a -> s{_caOutputLocation = a})

-- | The targets (either instances or tags) for the association.
caTargets :: Lens' CreateAssociation [Target]
caTargets = lens _caTargets (\ s a -> s{_caTargets = a}) . _Default . _Coerce

-- | The parameters for the documents runtime configuration.
caParameters :: Lens' CreateAssociation (HashMap Text [Text])
caParameters = lens _caParameters (\ s a -> s{_caParameters = a}) . _Default . _Map

-- | The document version you want to associate with the target(s). Can be a specific version or the default version.
caDocumentVersion :: Lens' CreateAssociation (Maybe Text)
caDocumentVersion = lens _caDocumentVersion (\ s a -> s{_caDocumentVersion = a})

-- | Specify a descriptive name for the association.
caAssociationName :: Lens' CreateAssociation (Maybe Text)
caAssociationName = lens _caAssociationName (\ s a -> s{_caAssociationName = a})

-- | The name of the Systems Manager document.
caName :: Lens' CreateAssociation Text
caName = lens _caName (\ s a -> s{_caName = a})

instance AWSRequest CreateAssociation where
        type Rs CreateAssociation = CreateAssociationResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 CreateAssociationResponse' <$>
                   (x .?> "AssociationDescription") <*>
                     (pure (fromEnum s)))

instance Hashable CreateAssociation where

instance NFData CreateAssociation where

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
                 [("InstanceId" .=) <$> _caInstanceId,
                  ("ScheduleExpression" .=) <$> _caScheduleExpression,
                  ("OutputLocation" .=) <$> _caOutputLocation,
                  ("Targets" .=) <$> _caTargets,
                  ("Parameters" .=) <$> _caParameters,
                  ("DocumentVersion" .=) <$> _caDocumentVersion,
                  ("AssociationName" .=) <$> _caAssociationName,
                  Just ("Name" .= _caName)])

instance ToPath CreateAssociation where
        toPath = const "/"

instance ToQuery CreateAssociation where
        toQuery = const mempty

-- | /See:/ 'createAssociationResponse' smart constructor.
data CreateAssociationResponse = CreateAssociationResponse'
  { _crsAssociationDescription :: !(Maybe AssociationDescription)
  , _crsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsAssociationDescription' - Information about the association.
--
-- * 'crsResponseStatus' - -- | The response status code.
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
crsAssociationDescription = lens _crsAssociationDescription (\ s a -> s{_crsAssociationDescription = a})

-- | -- | The response status code.
crsResponseStatus :: Lens' CreateAssociationResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a})

instance NFData CreateAssociationResponse where
