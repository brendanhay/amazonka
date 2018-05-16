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
-- Module      : Network.AWS.CloudFormation.EstimateTemplateCost
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the estimated monthly cost of a template. The return value is an AWS Simple Monthly Calculator URL with a query string that describes the resources required to run the template.
--
--
module Network.AWS.CloudFormation.EstimateTemplateCost
    (
    -- * Creating a Request
      estimateTemplateCost
    , EstimateTemplateCost
    -- * Request Lenses
    , etcParameters
    , etcTemplateBody
    , etcTemplateURL

    -- * Destructuring the Response
    , estimateTemplateCostResponse
    , EstimateTemplateCostResponse
    -- * Response Lenses
    , etcrsURL
    , etcrsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for an 'EstimateTemplateCost' action.
--
--
--
-- /See:/ 'estimateTemplateCost' smart constructor.
data EstimateTemplateCost = EstimateTemplateCost'
  { _etcParameters   :: !(Maybe [Parameter])
  , _etcTemplateBody :: !(Maybe Text)
  , _etcTemplateURL  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EstimateTemplateCost' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etcParameters' - A list of @Parameter@ structures that specify input parameters.
--
-- * 'etcTemplateBody' - Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. (For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.) Conditional: You must pass @TemplateBody@ or @TemplateURL@ . If both are passed, only @TemplateBody@ is used.
--
-- * 'etcTemplateURL' - Location of file containing the template body. The URL must point to a template that is located in an Amazon S3 bucket. For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide. Conditional: You must pass @TemplateURL@ or @TemplateBody@ . If both are passed, only @TemplateBody@ is used.
estimateTemplateCost
    :: EstimateTemplateCost
estimateTemplateCost =
  EstimateTemplateCost'
    { _etcParameters = Nothing
    , _etcTemplateBody = Nothing
    , _etcTemplateURL = Nothing
    }


-- | A list of @Parameter@ structures that specify input parameters.
etcParameters :: Lens' EstimateTemplateCost [Parameter]
etcParameters = lens _etcParameters (\ s a -> s{_etcParameters = a}) . _Default . _Coerce

-- | Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. (For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.) Conditional: You must pass @TemplateBody@ or @TemplateURL@ . If both are passed, only @TemplateBody@ is used.
etcTemplateBody :: Lens' EstimateTemplateCost (Maybe Text)
etcTemplateBody = lens _etcTemplateBody (\ s a -> s{_etcTemplateBody = a})

-- | Location of file containing the template body. The URL must point to a template that is located in an Amazon S3 bucket. For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide. Conditional: You must pass @TemplateURL@ or @TemplateBody@ . If both are passed, only @TemplateBody@ is used.
etcTemplateURL :: Lens' EstimateTemplateCost (Maybe Text)
etcTemplateURL = lens _etcTemplateURL (\ s a -> s{_etcTemplateURL = a})

instance AWSRequest EstimateTemplateCost where
        type Rs EstimateTemplateCost =
             EstimateTemplateCostResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "EstimateTemplateCostResult"
              (\ s h x ->
                 EstimateTemplateCostResponse' <$>
                   (x .@? "Url") <*> (pure (fromEnum s)))

instance Hashable EstimateTemplateCost where

instance NFData EstimateTemplateCost where

instance ToHeaders EstimateTemplateCost where
        toHeaders = const mempty

instance ToPath EstimateTemplateCost where
        toPath = const "/"

instance ToQuery EstimateTemplateCost where
        toQuery EstimateTemplateCost'{..}
          = mconcat
              ["Action" =: ("EstimateTemplateCost" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "Parameters" =:
                 toQuery (toQueryList "member" <$> _etcParameters),
               "TemplateBody" =: _etcTemplateBody,
               "TemplateURL" =: _etcTemplateURL]

-- | The output for a 'EstimateTemplateCost' action.
--
--
--
-- /See:/ 'estimateTemplateCostResponse' smart constructor.
data EstimateTemplateCostResponse = EstimateTemplateCostResponse'
  { _etcrsURL            :: !(Maybe Text)
  , _etcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EstimateTemplateCostResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etcrsURL' - An AWS Simple Monthly Calculator URL with a query string that describes the resources required to run the template.
--
-- * 'etcrsResponseStatus' - -- | The response status code.
estimateTemplateCostResponse
    :: Int -- ^ 'etcrsResponseStatus'
    -> EstimateTemplateCostResponse
estimateTemplateCostResponse pResponseStatus_ =
  EstimateTemplateCostResponse'
    {_etcrsURL = Nothing, _etcrsResponseStatus = pResponseStatus_}


-- | An AWS Simple Monthly Calculator URL with a query string that describes the resources required to run the template.
etcrsURL :: Lens' EstimateTemplateCostResponse (Maybe Text)
etcrsURL = lens _etcrsURL (\ s a -> s{_etcrsURL = a})

-- | -- | The response status code.
etcrsResponseStatus :: Lens' EstimateTemplateCostResponse Int
etcrsResponseStatus = lens _etcrsResponseStatus (\ s a -> s{_etcrsResponseStatus = a})

instance NFData EstimateTemplateCostResponse where
