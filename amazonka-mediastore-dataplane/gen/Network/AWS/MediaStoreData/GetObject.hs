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
-- Module      : Network.AWS.MediaStoreData.GetObject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads the object at the specified path.
--
--
module Network.AWS.MediaStoreData.GetObject
    (
    -- * Creating a Request
      getObject
    , GetObject
    -- * Request Lenses
    , goRange
    , goPath

    -- * Destructuring the Response
    , getObjectResponse
    , GetObjectResponse
    -- * Response Lenses
    , gorsETag
    , gorsContentLength
    , gorsCacheControl
    , gorsLastModified
    , gorsContentRange
    , gorsContentType
    , gorsStatusCode
    , gorsBody
    ) where

import Network.AWS.Lens
import Network.AWS.MediaStoreData.Types
import Network.AWS.MediaStoreData.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getObject' smart constructor.
data GetObject = GetObject'
  { _goRange :: !(Maybe Text)
  , _goPath  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goRange' - The range bytes of an object to retrieve. For more information about the @Range@ header, go to <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35> .
--
-- * 'goPath' - The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name> For example, to upload the file @mlaw.avi@ to the folder path @premium\canada@ in the container @movies@ , enter the path @premium/canada/mlaw.avi@ . Do not include the container name in this path. If the path includes any folders that don't exist yet, the service creates them. For example, suppose you have an existing @premium/usa@ subfolder. If you specify @premium/canada@ , the service creates a @canada@ subfolder in the @premium@ folder. You then have two subfolders, @usa@ and @canada@ , in the @premium@ folder.  There is no correlation between the path to the source and the path (folders) in the container in AWS Elemental MediaStore. For more information about folders and how they exist in a container, see the <http://docs.aws.amazon.com/mediastore/latest/ug/ AWS Elemental MediaStore User Guide> . The file name is the name that is assigned to the file that you upload. The file can have the same name inside and outside of AWS Elemental MediaStore, or it can have the same name. The file name can include or omit an extension.
getObject
    :: Text -- ^ 'goPath'
    -> GetObject
getObject pPath_ = GetObject' {_goRange = Nothing, _goPath = pPath_}


-- | The range bytes of an object to retrieve. For more information about the @Range@ header, go to <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35> .
goRange :: Lens' GetObject (Maybe Text)
goRange = lens _goRange (\ s a -> s{_goRange = a})

-- | The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name> For example, to upload the file @mlaw.avi@ to the folder path @premium\canada@ in the container @movies@ , enter the path @premium/canada/mlaw.avi@ . Do not include the container name in this path. If the path includes any folders that don't exist yet, the service creates them. For example, suppose you have an existing @premium/usa@ subfolder. If you specify @premium/canada@ , the service creates a @canada@ subfolder in the @premium@ folder. You then have two subfolders, @usa@ and @canada@ , in the @premium@ folder.  There is no correlation between the path to the source and the path (folders) in the container in AWS Elemental MediaStore. For more information about folders and how they exist in a container, see the <http://docs.aws.amazon.com/mediastore/latest/ug/ AWS Elemental MediaStore User Guide> . The file name is the name that is assigned to the file that you upload. The file can have the same name inside and outside of AWS Elemental MediaStore, or it can have the same name. The file name can include or omit an extension.
goPath :: Lens' GetObject Text
goPath = lens _goPath (\ s a -> s{_goPath = a})

instance AWSRequest GetObject where
        type Rs GetObject = GetObjectResponse
        request = get mediaStoreData
        response
          = receiveBody
              (\ s h x ->
                 GetObjectResponse' <$>
                   (h .#? "ETag") <*> (h .#? "Content-Length") <*>
                     (h .#? "Cache-Control")
                     <*> (h .#? "Last-Modified")
                     <*> (h .#? "Content-Range")
                     <*> (h .#? "Content-Type")
                     <*> (pure (fromEnum s))
                     <*> (pure x))

instance Hashable GetObject where

instance NFData GetObject where

instance ToHeaders GetObject where
        toHeaders GetObject'{..}
          = mconcat ["Range" =# _goRange]

instance ToPath GetObject where
        toPath GetObject'{..} = mconcat ["/", toBS _goPath]

instance ToQuery GetObject where
        toQuery = const mempty

-- | /See:/ 'getObjectResponse' smart constructor.
data GetObjectResponse = GetObjectResponse'
  { _gorsETag          :: !(Maybe Text)
  , _gorsContentLength :: !(Maybe Nat)
  , _gorsCacheControl  :: !(Maybe Text)
  , _gorsLastModified  :: !(Maybe POSIX)
  , _gorsContentRange  :: !(Maybe Text)
  , _gorsContentType   :: !(Maybe Text)
  , _gorsStatusCode    :: !Int
  , _gorsBody          :: !RsBody
  } deriving (Show, Generic)


-- | Creates a value of 'GetObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gorsETag' - The ETag that represents a unique instance of the object.
--
-- * 'gorsContentLength' - The length of the object in bytes.
--
-- * 'gorsCacheControl' - An optional @CacheControl@ header that allows the caller to control the object's cache behavior. Headers can be passed in as specified in the HTTP spec at <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> . Headers with a custom user-defined value are also accepted.
--
-- * 'gorsLastModified' - The date and time that the object was last modified.
--
-- * 'gorsContentRange' - The range of bytes to retrieve.
--
-- * 'gorsContentType' - The content type of the object.
--
-- * 'gorsStatusCode' - The HTML status code of the request. Status codes ranging from 200 to 299 indicate success. All other status codes indicate the type of error that occurred.
--
-- * 'gorsBody' - The bytes of the object.
getObjectResponse
    :: Int -- ^ 'gorsStatusCode'
    -> RsBody -- ^ 'gorsBody'
    -> GetObjectResponse
getObjectResponse pStatusCode_ pBody_ =
  GetObjectResponse'
    { _gorsETag = Nothing
    , _gorsContentLength = Nothing
    , _gorsCacheControl = Nothing
    , _gorsLastModified = Nothing
    , _gorsContentRange = Nothing
    , _gorsContentType = Nothing
    , _gorsStatusCode = pStatusCode_
    , _gorsBody = pBody_
    }


-- | The ETag that represents a unique instance of the object.
gorsETag :: Lens' GetObjectResponse (Maybe Text)
gorsETag = lens _gorsETag (\ s a -> s{_gorsETag = a})

-- | The length of the object in bytes.
gorsContentLength :: Lens' GetObjectResponse (Maybe Natural)
gorsContentLength = lens _gorsContentLength (\ s a -> s{_gorsContentLength = a}) . mapping _Nat

-- | An optional @CacheControl@ header that allows the caller to control the object's cache behavior. Headers can be passed in as specified in the HTTP spec at <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> . Headers with a custom user-defined value are also accepted.
gorsCacheControl :: Lens' GetObjectResponse (Maybe Text)
gorsCacheControl = lens _gorsCacheControl (\ s a -> s{_gorsCacheControl = a})

-- | The date and time that the object was last modified.
gorsLastModified :: Lens' GetObjectResponse (Maybe UTCTime)
gorsLastModified = lens _gorsLastModified (\ s a -> s{_gorsLastModified = a}) . mapping _Time

-- | The range of bytes to retrieve.
gorsContentRange :: Lens' GetObjectResponse (Maybe Text)
gorsContentRange = lens _gorsContentRange (\ s a -> s{_gorsContentRange = a})

-- | The content type of the object.
gorsContentType :: Lens' GetObjectResponse (Maybe Text)
gorsContentType = lens _gorsContentType (\ s a -> s{_gorsContentType = a})

-- | The HTML status code of the request. Status codes ranging from 200 to 299 indicate success. All other status codes indicate the type of error that occurred.
gorsStatusCode :: Lens' GetObjectResponse Int
gorsStatusCode = lens _gorsStatusCode (\ s a -> s{_gorsStatusCode = a})

-- | The bytes of the object.
gorsBody :: Lens' GetObjectResponse RsBody
gorsBody = lens _gorsBody (\ s a -> s{_gorsBody = a})
