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
-- Module      : Network.AWS.CloudFormation.ValidateTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates a specified template. AWS CloudFormation first checks if the template is valid JSON. If it isn't, AWS CloudFormation checks if the template is valid YAML. If both these checks fail, AWS CloudFormation returns a template validation error.
--
--
module Network.AWS.CloudFormation.ValidateTemplate
    (
    -- * Creating a Request
      validateTemplate
    , ValidateTemplate
    -- * Request Lenses
    , vtTemplateBody
    , vtTemplateURL

    -- * Destructuring the Response
    , validateTemplateResponse
    , ValidateTemplateResponse
    -- * Response Lenses
    , vtrsDeclaredTransforms
    , vtrsCapabilitiesReason
    , vtrsParameters
    , vtrsDescription
    , vtrsCapabilities
    , vtrsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for 'ValidateTemplate' action.
--
--
--
-- /See:/ 'validateTemplate' smart constructor.
data ValidateTemplate = ValidateTemplate'
  { _vtTemplateBody :: !(Maybe Text)
  , _vtTemplateURL  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ValidateTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vtTemplateBody' - Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide. Conditional: You must pass @TemplateURL@ or @TemplateBody@ . If both are passed, only @TemplateBody@ is used.
--
-- * 'vtTemplateURL' - Location of file containing the template body. The URL must point to a template (max size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide. Conditional: You must pass @TemplateURL@ or @TemplateBody@ . If both are passed, only @TemplateBody@ is used.
validateTemplate
    :: ValidateTemplate
validateTemplate =
  ValidateTemplate' {_vtTemplateBody = Nothing, _vtTemplateURL = Nothing}


-- | Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide. Conditional: You must pass @TemplateURL@ or @TemplateBody@ . If both are passed, only @TemplateBody@ is used.
vtTemplateBody :: Lens' ValidateTemplate (Maybe Text)
vtTemplateBody = lens _vtTemplateBody (\ s a -> s{_vtTemplateBody = a})

-- | Location of file containing the template body. The URL must point to a template (max size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide. Conditional: You must pass @TemplateURL@ or @TemplateBody@ . If both are passed, only @TemplateBody@ is used.
vtTemplateURL :: Lens' ValidateTemplate (Maybe Text)
vtTemplateURL = lens _vtTemplateURL (\ s a -> s{_vtTemplateURL = a})

instance AWSRequest ValidateTemplate where
        type Rs ValidateTemplate = ValidateTemplateResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "ValidateTemplateResult"
              (\ s h x ->
                 ValidateTemplateResponse' <$>
                   (x .@? "DeclaredTransforms" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "CapabilitiesReason")
                     <*>
                     (x .@? "Parameters" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (x .@? "Description")
                     <*>
                     (x .@? "Capabilities" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ValidateTemplate where

instance NFData ValidateTemplate where

instance ToHeaders ValidateTemplate where
        toHeaders = const mempty

instance ToPath ValidateTemplate where
        toPath = const "/"

instance ToQuery ValidateTemplate where
        toQuery ValidateTemplate'{..}
          = mconcat
              ["Action" =: ("ValidateTemplate" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "TemplateBody" =: _vtTemplateBody,
               "TemplateURL" =: _vtTemplateURL]

-- | The output for 'ValidateTemplate' action.
--
--
--
-- /See:/ 'validateTemplateResponse' smart constructor.
data ValidateTemplateResponse = ValidateTemplateResponse'
  { _vtrsDeclaredTransforms :: !(Maybe [Text])
  , _vtrsCapabilitiesReason :: !(Maybe Text)
  , _vtrsParameters         :: !(Maybe [TemplateParameter])
  , _vtrsDescription        :: !(Maybe Text)
  , _vtrsCapabilities       :: !(Maybe [Capability])
  , _vtrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ValidateTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vtrsDeclaredTransforms' - A list of the transforms that are declared in the template.
--
-- * 'vtrsCapabilitiesReason' - The list of resources that generated the values in the @Capabilities@ response element.
--
-- * 'vtrsParameters' - A list of @TemplateParameter@ structures.
--
-- * 'vtrsDescription' - The description found within the template.
--
-- * 'vtrsCapabilities' - The capabilities found within the template. If your template contains IAM resources, you must specify the CAPABILITY_IAM or CAPABILITY_NAMED_IAM value for this parameter when you use the 'CreateStack' or 'UpdateStack' actions with your template; otherwise, those actions return an InsufficientCapabilities error. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates> .
--
-- * 'vtrsResponseStatus' - -- | The response status code.
validateTemplateResponse
    :: Int -- ^ 'vtrsResponseStatus'
    -> ValidateTemplateResponse
validateTemplateResponse pResponseStatus_ =
  ValidateTemplateResponse'
    { _vtrsDeclaredTransforms = Nothing
    , _vtrsCapabilitiesReason = Nothing
    , _vtrsParameters = Nothing
    , _vtrsDescription = Nothing
    , _vtrsCapabilities = Nothing
    , _vtrsResponseStatus = pResponseStatus_
    }


-- | A list of the transforms that are declared in the template.
vtrsDeclaredTransforms :: Lens' ValidateTemplateResponse [Text]
vtrsDeclaredTransforms = lens _vtrsDeclaredTransforms (\ s a -> s{_vtrsDeclaredTransforms = a}) . _Default . _Coerce

-- | The list of resources that generated the values in the @Capabilities@ response element.
vtrsCapabilitiesReason :: Lens' ValidateTemplateResponse (Maybe Text)
vtrsCapabilitiesReason = lens _vtrsCapabilitiesReason (\ s a -> s{_vtrsCapabilitiesReason = a})

-- | A list of @TemplateParameter@ structures.
vtrsParameters :: Lens' ValidateTemplateResponse [TemplateParameter]
vtrsParameters = lens _vtrsParameters (\ s a -> s{_vtrsParameters = a}) . _Default . _Coerce

-- | The description found within the template.
vtrsDescription :: Lens' ValidateTemplateResponse (Maybe Text)
vtrsDescription = lens _vtrsDescription (\ s a -> s{_vtrsDescription = a})

-- | The capabilities found within the template. If your template contains IAM resources, you must specify the CAPABILITY_IAM or CAPABILITY_NAMED_IAM value for this parameter when you use the 'CreateStack' or 'UpdateStack' actions with your template; otherwise, those actions return an InsufficientCapabilities error. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates> .
vtrsCapabilities :: Lens' ValidateTemplateResponse [Capability]
vtrsCapabilities = lens _vtrsCapabilities (\ s a -> s{_vtrsCapabilities = a}) . _Default . _Coerce

-- | -- | The response status code.
vtrsResponseStatus :: Lens' ValidateTemplateResponse Int
vtrsResponseStatus = lens _vtrsResponseStatus (\ s a -> s{_vtrsResponseStatus = a})

instance NFData ValidateTemplateResponse where
