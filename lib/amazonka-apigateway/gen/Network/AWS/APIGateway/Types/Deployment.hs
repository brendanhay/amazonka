{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.Deployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.Deployment where

import Network.AWS.APIGateway.Types.MethodSnapshot
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An immutable representation of a 'RestApi' resource that can be called by users using 'Stages' . A deployment must be associated with a 'Stage' for it to be callable over the Internet.
--
--
-- To create a deployment, call @POST@ on the 'Deployments' resource of a 'RestApi' . To view, update, or delete a deployment, call @GET@ , @PATCH@ , or @DELETE@ on the specified deployment resource (@/restapis/{restapi_id}/deployments/{deployment_id}@ ).'RestApi' , 'Deployments' , 'Stage' , <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-deployment.html AWS CLI> , <https://aws.amazon.com/tools/ AWS SDKs>
--
-- /See:/ 'deployment' smart constructor.
data Deployment = Deployment'
  { _dApiSummary ::
      !(Maybe (Map Text (Map Text (MethodSnapshot)))),
    _dCreatedDate :: !(Maybe POSIX),
    _dId :: !(Maybe Text),
    _dDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Deployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dApiSummary' - A summary of the 'RestApi' at the date and time that the deployment resource was created.
--
-- * 'dCreatedDate' - The date and time that the deployment resource was created.
--
-- * 'dId' - The identifier for the deployment resource.
--
-- * 'dDescription' - The description for the deployment resource.
deployment ::
  Deployment
deployment =
  Deployment'
    { _dApiSummary = Nothing,
      _dCreatedDate = Nothing,
      _dId = Nothing,
      _dDescription = Nothing
    }

-- | A summary of the 'RestApi' at the date and time that the deployment resource was created.
dApiSummary :: Lens' Deployment (HashMap Text (HashMap Text (MethodSnapshot)))
dApiSummary = lens _dApiSummary (\s a -> s {_dApiSummary = a}) . _Default . _Map

-- | The date and time that the deployment resource was created.
dCreatedDate :: Lens' Deployment (Maybe UTCTime)
dCreatedDate = lens _dCreatedDate (\s a -> s {_dCreatedDate = a}) . mapping _Time

-- | The identifier for the deployment resource.
dId :: Lens' Deployment (Maybe Text)
dId = lens _dId (\s a -> s {_dId = a})

-- | The description for the deployment resource.
dDescription :: Lens' Deployment (Maybe Text)
dDescription = lens _dDescription (\s a -> s {_dDescription = a})

instance FromJSON Deployment where
  parseJSON =
    withObject
      "Deployment"
      ( \x ->
          Deployment'
            <$> (x .:? "apiSummary" .!= mempty)
            <*> (x .:? "createdDate")
            <*> (x .:? "id")
            <*> (x .:? "description")
      )

instance Hashable Deployment

instance NFData Deployment
