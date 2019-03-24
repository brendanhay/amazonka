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
-- Module      : Network.AWS.SSM.PutParameter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add a parameter to the system.
--
--
module Network.AWS.SSM.PutParameter
    (
    -- * Creating a Request
      putParameter
    , PutParameter
    -- * Request Lenses
    , ppKeyId
    , ppAllowedPattern
    , ppOverwrite
    , ppDescription
    , ppTags
    , ppName
    , ppValue
    , ppType

    -- * Destructuring the Response
    , putParameterResponse
    , PutParameterResponse
    -- * Response Lenses
    , pprsVersion
    , pprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'putParameter' smart constructor.
data PutParameter = PutParameter'
  { _ppKeyId          :: !(Maybe Text)
  , _ppAllowedPattern :: !(Maybe Text)
  , _ppOverwrite      :: !(Maybe Bool)
  , _ppDescription    :: !(Maybe Text)
  , _ppTags           :: !(Maybe [Tag])
  , _ppName           :: !Text
  , _ppValue          :: !Text
  , _ppType           :: !ParameterType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppKeyId' - The KMS Key ID that you want to use to encrypt a parameter. Either the default AWS Key Management Service (AWS KMS) key automatically assigned to your AWS account or a custom key. Required for parameters that use the @SecureString@ data type. If you don't specify a key ID, the system uses the default key associated with your AWS account.     * To use your default AWS KMS key, choose the @SecureString@ data type, and do /not/ specify the @Key ID@ when you create the parameter. The system automatically populates @Key ID@ with your default KMS key.     * To use a custom KMS key, choose the @SecureString@ data type with the @Key ID@ parameter.
--
-- * 'ppAllowedPattern' - A regular expression used to validate the parameter value. For example, for String types with values restricted to numbers, you can specify the following: AllowedPattern=^\d+$
--
-- * 'ppOverwrite' - Overwrite an existing parameter. If not specified, will default to "false".
--
-- * 'ppDescription' - Information about the parameter that you want to add to the system. Optional but recommended. /Important:/ Do not enter personally identifiable information in this field.
--
-- * 'ppTags' - Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag a Systems Manager parameter to identify the type of resource to which it applies, the environment, or the type of configuration data referenced by the parameter. In this case, you could specify the following key name/value pairs:     * @Key=Resource,Value=S3bucket@      * @Key=OS,Value=Windows@      * @Key=ParameterType,Value=LicenseKey@
--
-- * 'ppName' - The fully qualified name of the parameter that you want to add to the system. The fully qualified name includes the complete hierarchy of the parameter path and name. For example: @/Dev/DBServer/MySQL/db-string13@  Naming Constraints:     * Parameter names are case sensitive.     * A parameter name must be unique within an AWS Region     * A parameter name can't be prefixed with "aws" or "ssm" (case-insensitive).     * Parameter names can include only the following symbols and letters: @a-zA-Z0-9_.-/@      * A parameter name can't include spaces.     * Parameter hierarchies are limited to a maximum depth of fifteen levels. For additional information about valid values for parameter names, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-parameter-name-constraints.html Requirements and Constraints for Parameter Names> in the /AWS Systems Manager User Guide/ .
--
-- * 'ppValue' - The parameter value that you want to add to the system.
--
-- * 'ppType' - The type of parameter that you want to add to the system. Items in a @StringList@ must be separated by a comma (,). You can't use other punctuation or special character to escape items in the list. If you have a parameter value that requires a comma, then use the @String@ data type.
putParameter
    :: Text -- ^ 'ppName'
    -> Text -- ^ 'ppValue'
    -> ParameterType -- ^ 'ppType'
    -> PutParameter
putParameter pName_ pValue_ pType_ =
  PutParameter'
    { _ppKeyId = Nothing
    , _ppAllowedPattern = Nothing
    , _ppOverwrite = Nothing
    , _ppDescription = Nothing
    , _ppTags = Nothing
    , _ppName = pName_
    , _ppValue = pValue_
    , _ppType = pType_
    }


-- | The KMS Key ID that you want to use to encrypt a parameter. Either the default AWS Key Management Service (AWS KMS) key automatically assigned to your AWS account or a custom key. Required for parameters that use the @SecureString@ data type. If you don't specify a key ID, the system uses the default key associated with your AWS account.     * To use your default AWS KMS key, choose the @SecureString@ data type, and do /not/ specify the @Key ID@ when you create the parameter. The system automatically populates @Key ID@ with your default KMS key.     * To use a custom KMS key, choose the @SecureString@ data type with the @Key ID@ parameter.
ppKeyId :: Lens' PutParameter (Maybe Text)
ppKeyId = lens _ppKeyId (\ s a -> s{_ppKeyId = a})

-- | A regular expression used to validate the parameter value. For example, for String types with values restricted to numbers, you can specify the following: AllowedPattern=^\d+$
ppAllowedPattern :: Lens' PutParameter (Maybe Text)
ppAllowedPattern = lens _ppAllowedPattern (\ s a -> s{_ppAllowedPattern = a})

-- | Overwrite an existing parameter. If not specified, will default to "false".
ppOverwrite :: Lens' PutParameter (Maybe Bool)
ppOverwrite = lens _ppOverwrite (\ s a -> s{_ppOverwrite = a})

-- | Information about the parameter that you want to add to the system. Optional but recommended. /Important:/ Do not enter personally identifiable information in this field.
ppDescription :: Lens' PutParameter (Maybe Text)
ppDescription = lens _ppDescription (\ s a -> s{_ppDescription = a})

-- | Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag a Systems Manager parameter to identify the type of resource to which it applies, the environment, or the type of configuration data referenced by the parameter. In this case, you could specify the following key name/value pairs:     * @Key=Resource,Value=S3bucket@      * @Key=OS,Value=Windows@      * @Key=ParameterType,Value=LicenseKey@
ppTags :: Lens' PutParameter [Tag]
ppTags = lens _ppTags (\ s a -> s{_ppTags = a}) . _Default . _Coerce

-- | The fully qualified name of the parameter that you want to add to the system. The fully qualified name includes the complete hierarchy of the parameter path and name. For example: @/Dev/DBServer/MySQL/db-string13@  Naming Constraints:     * Parameter names are case sensitive.     * A parameter name must be unique within an AWS Region     * A parameter name can't be prefixed with "aws" or "ssm" (case-insensitive).     * Parameter names can include only the following symbols and letters: @a-zA-Z0-9_.-/@      * A parameter name can't include spaces.     * Parameter hierarchies are limited to a maximum depth of fifteen levels. For additional information about valid values for parameter names, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-parameter-name-constraints.html Requirements and Constraints for Parameter Names> in the /AWS Systems Manager User Guide/ .
ppName :: Lens' PutParameter Text
ppName = lens _ppName (\ s a -> s{_ppName = a})

-- | The parameter value that you want to add to the system.
ppValue :: Lens' PutParameter Text
ppValue = lens _ppValue (\ s a -> s{_ppValue = a})

-- | The type of parameter that you want to add to the system. Items in a @StringList@ must be separated by a comma (,). You can't use other punctuation or special character to escape items in the list. If you have a parameter value that requires a comma, then use the @String@ data type.
ppType :: Lens' PutParameter ParameterType
ppType = lens _ppType (\ s a -> s{_ppType = a})

instance AWSRequest PutParameter where
        type Rs PutParameter = PutParameterResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 PutParameterResponse' <$>
                   (x .?> "Version") <*> (pure (fromEnum s)))

instance Hashable PutParameter where

instance NFData PutParameter where

instance ToHeaders PutParameter where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.PutParameter" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutParameter where
        toJSON PutParameter'{..}
          = object
              (catMaybes
                 [("KeyId" .=) <$> _ppKeyId,
                  ("AllowedPattern" .=) <$> _ppAllowedPattern,
                  ("Overwrite" .=) <$> _ppOverwrite,
                  ("Description" .=) <$> _ppDescription,
                  ("Tags" .=) <$> _ppTags, Just ("Name" .= _ppName),
                  Just ("Value" .= _ppValue),
                  Just ("Type" .= _ppType)])

instance ToPath PutParameter where
        toPath = const "/"

instance ToQuery PutParameter where
        toQuery = const mempty

-- | /See:/ 'putParameterResponse' smart constructor.
data PutParameterResponse = PutParameterResponse'
  { _pprsVersion        :: !(Maybe Integer)
  , _pprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutParameterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pprsVersion' - The new version number of a parameter. If you edit a parameter value, Parameter Store automatically creates a new version and assigns this new version a unique ID. You can reference a parameter version ID in API actions or in Systems Manager documents (SSM documents). By default, if you don't specify a specific version, the system returns the latest parameter value when a parameter is called.
--
-- * 'pprsResponseStatus' - -- | The response status code.
putParameterResponse
    :: Int -- ^ 'pprsResponseStatus'
    -> PutParameterResponse
putParameterResponse pResponseStatus_ =
  PutParameterResponse'
    {_pprsVersion = Nothing, _pprsResponseStatus = pResponseStatus_}


-- | The new version number of a parameter. If you edit a parameter value, Parameter Store automatically creates a new version and assigns this new version a unique ID. You can reference a parameter version ID in API actions or in Systems Manager documents (SSM documents). By default, if you don't specify a specific version, the system returns the latest parameter value when a parameter is called.
pprsVersion :: Lens' PutParameterResponse (Maybe Integer)
pprsVersion = lens _pprsVersion (\ s a -> s{_pprsVersion = a})

-- | -- | The response status code.
pprsResponseStatus :: Lens' PutParameterResponse Int
pprsResponseStatus = lens _pprsResponseStatus (\ s a -> s{_pprsResponseStatus = a})

instance NFData PutParameterResponse where
