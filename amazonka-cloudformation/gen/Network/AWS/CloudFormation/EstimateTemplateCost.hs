{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.EstimateTemplateCost
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the estimated monthly cost of a template. The return value is an
-- AWS Simple Monthly Calculator URL with a query string that describes the
-- resources required to run the template.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_EstimateTemplateCost.html>
module Network.AWS.CloudFormation.EstimateTemplateCost
    (
    -- * Request
      EstimateTemplateCost
    -- ** Request constructor
    , estimateTemplateCost
    -- ** Request lenses
    , etcParameters
    , etcTemplateBody
    , etcTemplateURL

    -- * Response
    , EstimateTemplateCostResponse
    -- ** Response constructor
    , estimateTemplateCostResponse
    -- ** Response lenses
    , etcrsURL
    , etcrsStatus
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'estimateTemplateCost' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'etcParameters'
--
-- * 'etcTemplateBody'
--
-- * 'etcTemplateURL'
data EstimateTemplateCost = EstimateTemplateCost'
    { _etcParameters   :: !(Maybe [Parameter])
    , _etcTemplateBody :: !(Maybe Text)
    , _etcTemplateURL  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EstimateTemplateCost' smart constructor.
estimateTemplateCost :: EstimateTemplateCost
estimateTemplateCost =
    EstimateTemplateCost'
    { _etcParameters = Nothing
    , _etcTemplateBody = Nothing
    , _etcTemplateURL = Nothing
    }

-- | A list of @Parameter@ structures that specify input parameters.
etcParameters :: Lens' EstimateTemplateCost [Parameter]
etcParameters = lens _etcParameters (\ s a -> s{_etcParameters = a}) . _Default . _Coerce;

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. (For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.)
--
-- Conditional: You must pass @TemplateBody@ or @TemplateURL@. If both are
-- passed, only @TemplateBody@ is used.
etcTemplateBody :: Lens' EstimateTemplateCost (Maybe Text)
etcTemplateBody = lens _etcTemplateBody (\ s a -> s{_etcTemplateBody = a});

-- | Location of file containing the template body. The URL must point to a
-- template located in an S3 bucket in the same region as the stack. For
-- more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must pass @TemplateURL@ or @TemplateBody@. If both are
-- passed, only @TemplateBody@ is used.
etcTemplateURL :: Lens' EstimateTemplateCost (Maybe Text)
etcTemplateURL = lens _etcTemplateURL (\ s a -> s{_etcTemplateURL = a});

instance AWSRequest EstimateTemplateCost where
        type Sv EstimateTemplateCost = CloudFormation
        type Rs EstimateTemplateCost =
             EstimateTemplateCostResponse
        request = postQuery
        response
          = receiveXMLWrapper "EstimateTemplateCostResult"
              (\ s h x ->
                 EstimateTemplateCostResponse' <$>
                   (x .@? "Url") <*> (pure (fromEnum s)))

instance ToHeaders EstimateTemplateCost where
        toHeaders = const mempty

instance ToPath EstimateTemplateCost where
        toPath = const mempty

instance ToQuery EstimateTemplateCost where
        toQuery EstimateTemplateCost'{..}
          = mconcat
              ["Action" =: ("EstimateTemplateCost" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "Parameters" =:
                 toQuery (toQueryList "member" <$> _etcParameters),
               "TemplateBody" =: _etcTemplateBody,
               "TemplateURL" =: _etcTemplateURL]

-- | The output for a EstimateTemplateCost action.
--
-- /See:/ 'estimateTemplateCostResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'etcrsURL'
--
-- * 'etcrsStatus'
data EstimateTemplateCostResponse = EstimateTemplateCostResponse'
    { _etcrsURL    :: !(Maybe Text)
    , _etcrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EstimateTemplateCostResponse' smart constructor.
estimateTemplateCostResponse :: Int -> EstimateTemplateCostResponse
estimateTemplateCostResponse pStatus_ =
    EstimateTemplateCostResponse'
    { _etcrsURL = Nothing
    , _etcrsStatus = pStatus_
    }

-- | An AWS Simple Monthly Calculator URL with a query string that describes
-- the resources required to run the template.
etcrsURL :: Lens' EstimateTemplateCostResponse (Maybe Text)
etcrsURL = lens _etcrsURL (\ s a -> s{_etcrsURL = a});

-- | FIXME: Undocumented member.
etcrsStatus :: Lens' EstimateTemplateCostResponse Int
etcrsStatus = lens _etcrsStatus (\ s a -> s{_etcrsStatus = a});
