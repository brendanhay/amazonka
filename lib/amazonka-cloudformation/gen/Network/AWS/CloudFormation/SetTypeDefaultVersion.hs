{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.SetTypeDefaultVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specify the default version of a type. The default version of a type will be used in CloudFormation operations.
module Network.AWS.CloudFormation.SetTypeDefaultVersion
  ( -- * Creating a Request
    setTypeDefaultVersion,
    SetTypeDefaultVersion,

    -- * Request Lenses
    stdvVersionId,
    stdvTypeName,
    stdvARN,
    stdvType,

    -- * Destructuring the Response
    setTypeDefaultVersionResponse,
    SetTypeDefaultVersionResponse,

    -- * Response Lenses
    stdvrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setTypeDefaultVersion' smart constructor.
data SetTypeDefaultVersion = SetTypeDefaultVersion'
  { _stdvVersionId ::
      !(Maybe Text),
    _stdvTypeName :: !(Maybe Text),
    _stdvARN :: !(Maybe Text),
    _stdvType :: !(Maybe RegistryType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SetTypeDefaultVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stdvVersionId' - The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
--
-- * 'stdvTypeName' - The name of the type. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- * 'stdvARN' - The Amazon Resource Name (ARN) of the type for which you want version summary information. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- * 'stdvType' - The kind of type. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
setTypeDefaultVersion ::
  SetTypeDefaultVersion
setTypeDefaultVersion =
  SetTypeDefaultVersion'
    { _stdvVersionId = Nothing,
      _stdvTypeName = Nothing,
      _stdvARN = Nothing,
      _stdvType = Nothing
    }

-- | The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
stdvVersionId :: Lens' SetTypeDefaultVersion (Maybe Text)
stdvVersionId = lens _stdvVersionId (\s a -> s {_stdvVersionId = a})

-- | The name of the type. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
stdvTypeName :: Lens' SetTypeDefaultVersion (Maybe Text)
stdvTypeName = lens _stdvTypeName (\s a -> s {_stdvTypeName = a})

-- | The Amazon Resource Name (ARN) of the type for which you want version summary information. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
stdvARN :: Lens' SetTypeDefaultVersion (Maybe Text)
stdvARN = lens _stdvARN (\s a -> s {_stdvARN = a})

-- | The kind of type. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
stdvType :: Lens' SetTypeDefaultVersion (Maybe RegistryType)
stdvType = lens _stdvType (\s a -> s {_stdvType = a})

instance AWSRequest SetTypeDefaultVersion where
  type Rs SetTypeDefaultVersion = SetTypeDefaultVersionResponse
  request = postQuery cloudFormation
  response =
    receiveXMLWrapper
      "SetTypeDefaultVersionResult"
      (\s h x -> SetTypeDefaultVersionResponse' <$> (pure (fromEnum s)))

instance Hashable SetTypeDefaultVersion

instance NFData SetTypeDefaultVersion

instance ToHeaders SetTypeDefaultVersion where
  toHeaders = const mempty

instance ToPath SetTypeDefaultVersion where
  toPath = const "/"

instance ToQuery SetTypeDefaultVersion where
  toQuery SetTypeDefaultVersion' {..} =
    mconcat
      [ "Action" =: ("SetTypeDefaultVersion" :: ByteString),
        "Version" =: ("2010-05-15" :: ByteString),
        "VersionId" =: _stdvVersionId,
        "TypeName" =: _stdvTypeName,
        "Arn" =: _stdvARN,
        "Type" =: _stdvType
      ]

-- | /See:/ 'setTypeDefaultVersionResponse' smart constructor.
newtype SetTypeDefaultVersionResponse = SetTypeDefaultVersionResponse'
  { _stdvrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SetTypeDefaultVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stdvrsResponseStatus' - -- | The response status code.
setTypeDefaultVersionResponse ::
  -- | 'stdvrsResponseStatus'
  Int ->
  SetTypeDefaultVersionResponse
setTypeDefaultVersionResponse pResponseStatus_ =
  SetTypeDefaultVersionResponse'
    { _stdvrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
stdvrsResponseStatus :: Lens' SetTypeDefaultVersionResponse Int
stdvrsResponseStatus = lens _stdvrsResponseStatus (\s a -> s {_stdvrsResponseStatus = a})

instance NFData SetTypeDefaultVersionResponse
