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
  , _ppName           :: !Text
  , _ppValue          :: !Text
  , _ppType           :: !ParameterType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppKeyId' - The KMS Key ID that you want to use to encrypt a parameter when you choose the SecureString data type. If you don't specify a key ID, the system uses the default key associated with your AWS account.
--
-- * 'ppAllowedPattern' - A regular expression used to validate the parameter value. For example, for String types with values restricted to numbers, you can specify the following: AllowedPattern=^\d+$
--
-- * 'ppOverwrite' - Overwrite an existing parameter. If not specified, will default to "false".
--
-- * 'ppDescription' - Information about the parameter that you want to add to the system. /Important:/ Do not enter personally identifiable information in this field.
--
-- * 'ppName' - The fully qualified name of the parameter that you want to add to the system. The fully qualified name includes the complete hierarchy of the parameter path and name. For example: @/Dev/DBServer/MySQL/db-string13@  For information about parameter name requirements and restrictions, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-su-create.html#sysman-paramstore-su-create-about About Creating Systems Manager Parameters> in the /AWS Systems Manager User Guide/ .
--
-- * 'ppValue' - The parameter value that you want to add to the system.
--
-- * 'ppType' - The type of parameter that you want to add to the system.
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
    , _ppName = pName_
    , _ppValue = pValue_
    , _ppType = pType_
    }


-- | The KMS Key ID that you want to use to encrypt a parameter when you choose the SecureString data type. If you don't specify a key ID, the system uses the default key associated with your AWS account.
ppKeyId :: Lens' PutParameter (Maybe Text)
ppKeyId = lens _ppKeyId (\ s a -> s{_ppKeyId = a})

-- | A regular expression used to validate the parameter value. For example, for String types with values restricted to numbers, you can specify the following: AllowedPattern=^\d+$
ppAllowedPattern :: Lens' PutParameter (Maybe Text)
ppAllowedPattern = lens _ppAllowedPattern (\ s a -> s{_ppAllowedPattern = a})

-- | Overwrite an existing parameter. If not specified, will default to "false".
ppOverwrite :: Lens' PutParameter (Maybe Bool)
ppOverwrite = lens _ppOverwrite (\ s a -> s{_ppOverwrite = a})

-- | Information about the parameter that you want to add to the system. /Important:/ Do not enter personally identifiable information in this field.
ppDescription :: Lens' PutParameter (Maybe Text)
ppDescription = lens _ppDescription (\ s a -> s{_ppDescription = a})

-- | The fully qualified name of the parameter that you want to add to the system. The fully qualified name includes the complete hierarchy of the parameter path and name. For example: @/Dev/DBServer/MySQL/db-string13@  For information about parameter name requirements and restrictions, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-su-create.html#sysman-paramstore-su-create-about About Creating Systems Manager Parameters> in the /AWS Systems Manager User Guide/ .
ppName :: Lens' PutParameter Text
ppName = lens _ppName (\ s a -> s{_ppName = a})

-- | The parameter value that you want to add to the system.
ppValue :: Lens' PutParameter Text
ppValue = lens _ppValue (\ s a -> s{_ppValue = a})

-- | The type of parameter that you want to add to the system.
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
                  Just ("Name" .= _ppName), Just ("Value" .= _ppValue),
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
