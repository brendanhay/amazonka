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
-- Module      : Network.AWS.CloudDirectory.ApplySchema
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the input published schema, at the specified version, into the 'Directory' with the same name and version as that of the published schema.
--
--
module Network.AWS.CloudDirectory.ApplySchema
    (
    -- * Creating a Request
      applySchema
    , ApplySchema
    -- * Request Lenses
    , asPublishedSchemaARN
    , asDirectoryARN

    -- * Destructuring the Response
    , applySchemaResponse
    , ApplySchemaResponse
    -- * Response Lenses
    , asrsDirectoryARN
    , asrsAppliedSchemaARN
    , asrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'applySchema' smart constructor.
data ApplySchema = ApplySchema'
  { _asPublishedSchemaARN :: !Text
  , _asDirectoryARN       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplySchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asPublishedSchemaARN' - Published schema Amazon Resource Name (ARN) that needs to be copied. For more information, see 'arns' .
--
-- * 'asDirectoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' into which the schema is copied. For more information, see 'arns' .
applySchema
    :: Text -- ^ 'asPublishedSchemaARN'
    -> Text -- ^ 'asDirectoryARN'
    -> ApplySchema
applySchema pPublishedSchemaARN_ pDirectoryARN_ =
  ApplySchema'
    { _asPublishedSchemaARN = pPublishedSchemaARN_
    , _asDirectoryARN = pDirectoryARN_
    }


-- | Published schema Amazon Resource Name (ARN) that needs to be copied. For more information, see 'arns' .
asPublishedSchemaARN :: Lens' ApplySchema Text
asPublishedSchemaARN = lens _asPublishedSchemaARN (\ s a -> s{_asPublishedSchemaARN = a})

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' into which the schema is copied. For more information, see 'arns' .
asDirectoryARN :: Lens' ApplySchema Text
asDirectoryARN = lens _asDirectoryARN (\ s a -> s{_asDirectoryARN = a})

instance AWSRequest ApplySchema where
        type Rs ApplySchema = ApplySchemaResponse
        request = putJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 ApplySchemaResponse' <$>
                   (x .?> "DirectoryArn") <*> (x .?> "AppliedSchemaArn")
                     <*> (pure (fromEnum s)))

instance Hashable ApplySchema where

instance NFData ApplySchema where

instance ToHeaders ApplySchema where
        toHeaders ApplySchema'{..}
          = mconcat ["x-amz-data-partition" =# _asDirectoryARN]

instance ToJSON ApplySchema where
        toJSON ApplySchema'{..}
          = object
              (catMaybes
                 [Just
                    ("PublishedSchemaArn" .= _asPublishedSchemaARN)])

instance ToPath ApplySchema where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/schema/apply"

instance ToQuery ApplySchema where
        toQuery = const mempty

-- | /See:/ 'applySchemaResponse' smart constructor.
data ApplySchemaResponse = ApplySchemaResponse'
  { _asrsDirectoryARN     :: !(Maybe Text)
  , _asrsAppliedSchemaARN :: !(Maybe Text)
  , _asrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplySchemaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asrsDirectoryARN' - The ARN that is associated with the 'Directory' . For more information, see 'arns' .
--
-- * 'asrsAppliedSchemaARN' - The applied schema ARN that is associated with the copied schema in the 'Directory' . You can use this ARN to describe the schema information applied on this directory. For more information, see 'arns' .
--
-- * 'asrsResponseStatus' - -- | The response status code.
applySchemaResponse
    :: Int -- ^ 'asrsResponseStatus'
    -> ApplySchemaResponse
applySchemaResponse pResponseStatus_ =
  ApplySchemaResponse'
    { _asrsDirectoryARN = Nothing
    , _asrsAppliedSchemaARN = Nothing
    , _asrsResponseStatus = pResponseStatus_
    }


-- | The ARN that is associated with the 'Directory' . For more information, see 'arns' .
asrsDirectoryARN :: Lens' ApplySchemaResponse (Maybe Text)
asrsDirectoryARN = lens _asrsDirectoryARN (\ s a -> s{_asrsDirectoryARN = a})

-- | The applied schema ARN that is associated with the copied schema in the 'Directory' . You can use this ARN to describe the schema information applied on this directory. For more information, see 'arns' .
asrsAppliedSchemaARN :: Lens' ApplySchemaResponse (Maybe Text)
asrsAppliedSchemaARN = lens _asrsAppliedSchemaARN (\ s a -> s{_asrsAppliedSchemaARN = a})

-- | -- | The response status code.
asrsResponseStatus :: Lens' ApplySchemaResponse Int
asrsResponseStatus = lens _asrsResponseStatus (\ s a -> s{_asrsResponseStatus = a})

instance NFData ApplySchemaResponse where
