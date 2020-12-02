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
-- Module      : Network.AWS.SSM.UpdateAssociation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an association. You can update the association name and version, the document version, schedule, parameters, and Amazon S3 output.
--
--
module Network.AWS.SSM.UpdateAssociation
    (
    -- * Creating a Request
      updateAssociation
    , UpdateAssociation
    -- * Request Lenses
    , uaScheduleExpression
    , uaName
    , uaOutputLocation
    , uaTargets
    , uaParameters
    , uaDocumentVersion
    , uaAssociationVersion
    , uaAssociationName
    , uaAssociationId

    -- * Destructuring the Response
    , updateAssociationResponse
    , UpdateAssociationResponse
    -- * Response Lenses
    , uarsAssociationDescription
    , uarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'updateAssociation' smart constructor.
data UpdateAssociation = UpdateAssociation'
  { _uaScheduleExpression :: !(Maybe Text)
  , _uaName               :: !(Maybe Text)
  , _uaOutputLocation     :: !(Maybe InstanceAssociationOutputLocation)
  , _uaTargets            :: !(Maybe [Target])
  , _uaParameters         :: !(Maybe (Map Text [Text]))
  , _uaDocumentVersion    :: !(Maybe Text)
  , _uaAssociationVersion :: !(Maybe Text)
  , _uaAssociationName    :: !(Maybe Text)
  , _uaAssociationId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaScheduleExpression' - The cron expression used to schedule the association that you want to update.
--
-- * 'uaName' - The name of the association document.
--
-- * 'uaOutputLocation' - An Amazon S3 bucket where you want to store the results of this request.
--
-- * 'uaTargets' - The targets of the association.
--
-- * 'uaParameters' - The parameters you want to update for the association. If you create a parameter using Parameter Store, you can reference the parameter using {{ssm:parameter-name}}
--
-- * 'uaDocumentVersion' - The document version you want update for the association.
--
-- * 'uaAssociationVersion' - This parameter is provided for concurrency control purposes. You must specify the latest association version in the service. If you want to ensure that this request succeeds, either specify @> LATEST@ , or omit this parameter.
--
-- * 'uaAssociationName' - The name of the association that you want to update.
--
-- * 'uaAssociationId' - The ID of the association you want to update.
updateAssociation
    :: Text -- ^ 'uaAssociationId'
    -> UpdateAssociation
updateAssociation pAssociationId_ =
  UpdateAssociation'
    { _uaScheduleExpression = Nothing
    , _uaName = Nothing
    , _uaOutputLocation = Nothing
    , _uaTargets = Nothing
    , _uaParameters = Nothing
    , _uaDocumentVersion = Nothing
    , _uaAssociationVersion = Nothing
    , _uaAssociationName = Nothing
    , _uaAssociationId = pAssociationId_
    }


-- | The cron expression used to schedule the association that you want to update.
uaScheduleExpression :: Lens' UpdateAssociation (Maybe Text)
uaScheduleExpression = lens _uaScheduleExpression (\ s a -> s{_uaScheduleExpression = a})

-- | The name of the association document.
uaName :: Lens' UpdateAssociation (Maybe Text)
uaName = lens _uaName (\ s a -> s{_uaName = a})

-- | An Amazon S3 bucket where you want to store the results of this request.
uaOutputLocation :: Lens' UpdateAssociation (Maybe InstanceAssociationOutputLocation)
uaOutputLocation = lens _uaOutputLocation (\ s a -> s{_uaOutputLocation = a})

-- | The targets of the association.
uaTargets :: Lens' UpdateAssociation [Target]
uaTargets = lens _uaTargets (\ s a -> s{_uaTargets = a}) . _Default . _Coerce

-- | The parameters you want to update for the association. If you create a parameter using Parameter Store, you can reference the parameter using {{ssm:parameter-name}}
uaParameters :: Lens' UpdateAssociation (HashMap Text [Text])
uaParameters = lens _uaParameters (\ s a -> s{_uaParameters = a}) . _Default . _Map

-- | The document version you want update for the association.
uaDocumentVersion :: Lens' UpdateAssociation (Maybe Text)
uaDocumentVersion = lens _uaDocumentVersion (\ s a -> s{_uaDocumentVersion = a})

-- | This parameter is provided for concurrency control purposes. You must specify the latest association version in the service. If you want to ensure that this request succeeds, either specify @> LATEST@ , or omit this parameter.
uaAssociationVersion :: Lens' UpdateAssociation (Maybe Text)
uaAssociationVersion = lens _uaAssociationVersion (\ s a -> s{_uaAssociationVersion = a})

-- | The name of the association that you want to update.
uaAssociationName :: Lens' UpdateAssociation (Maybe Text)
uaAssociationName = lens _uaAssociationName (\ s a -> s{_uaAssociationName = a})

-- | The ID of the association you want to update.
uaAssociationId :: Lens' UpdateAssociation Text
uaAssociationId = lens _uaAssociationId (\ s a -> s{_uaAssociationId = a})

instance AWSRequest UpdateAssociation where
        type Rs UpdateAssociation = UpdateAssociationResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 UpdateAssociationResponse' <$>
                   (x .?> "AssociationDescription") <*>
                     (pure (fromEnum s)))

instance Hashable UpdateAssociation where

instance NFData UpdateAssociation where

instance ToHeaders UpdateAssociation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.UpdateAssociation" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateAssociation where
        toJSON UpdateAssociation'{..}
          = object
              (catMaybes
                 [("ScheduleExpression" .=) <$> _uaScheduleExpression,
                  ("Name" .=) <$> _uaName,
                  ("OutputLocation" .=) <$> _uaOutputLocation,
                  ("Targets" .=) <$> _uaTargets,
                  ("Parameters" .=) <$> _uaParameters,
                  ("DocumentVersion" .=) <$> _uaDocumentVersion,
                  ("AssociationVersion" .=) <$> _uaAssociationVersion,
                  ("AssociationName" .=) <$> _uaAssociationName,
                  Just ("AssociationId" .= _uaAssociationId)])

instance ToPath UpdateAssociation where
        toPath = const "/"

instance ToQuery UpdateAssociation where
        toQuery = const mempty

-- | /See:/ 'updateAssociationResponse' smart constructor.
data UpdateAssociationResponse = UpdateAssociationResponse'
  { _uarsAssociationDescription :: !(Maybe AssociationDescription)
  , _uarsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uarsAssociationDescription' - The description of the association that was updated.
--
-- * 'uarsResponseStatus' - -- | The response status code.
updateAssociationResponse
    :: Int -- ^ 'uarsResponseStatus'
    -> UpdateAssociationResponse
updateAssociationResponse pResponseStatus_ =
  UpdateAssociationResponse'
    { _uarsAssociationDescription = Nothing
    , _uarsResponseStatus = pResponseStatus_
    }


-- | The description of the association that was updated.
uarsAssociationDescription :: Lens' UpdateAssociationResponse (Maybe AssociationDescription)
uarsAssociationDescription = lens _uarsAssociationDescription (\ s a -> s{_uarsAssociationDescription = a})

-- | -- | The response status code.
uarsResponseStatus :: Lens' UpdateAssociationResponse Int
uarsResponseStatus = lens _uarsResponseStatus (\ s a -> s{_uarsResponseStatus = a})

instance NFData UpdateAssociationResponse where
