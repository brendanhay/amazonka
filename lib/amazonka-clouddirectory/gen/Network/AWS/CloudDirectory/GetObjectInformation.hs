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
-- Module      : Network.AWS.CloudDirectory.GetObjectInformation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata about an object.
--
--
module Network.AWS.CloudDirectory.GetObjectInformation
    (
    -- * Creating a Request
      getObjectInformation
    , GetObjectInformation
    -- * Request Lenses
    , goiConsistencyLevel
    , goiDirectoryARN
    , goiObjectReference

    -- * Destructuring the Response
    , getObjectInformationResponse
    , GetObjectInformationResponse
    -- * Response Lenses
    , goirsObjectIdentifier
    , goirsSchemaFacets
    , goirsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getObjectInformation' smart constructor.
data GetObjectInformation = GetObjectInformation'
  { _goiConsistencyLevel :: !(Maybe ConsistencyLevel)
  , _goiDirectoryARN     :: !Text
  , _goiObjectReference  :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetObjectInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goiConsistencyLevel' - The consistency level at which to retrieve the object information.
--
-- * 'goiDirectoryARN' - The ARN of the directory being retrieved.
--
-- * 'goiObjectReference' - A reference to the object.
getObjectInformation
    :: Text -- ^ 'goiDirectoryARN'
    -> ObjectReference -- ^ 'goiObjectReference'
    -> GetObjectInformation
getObjectInformation pDirectoryARN_ pObjectReference_ =
  GetObjectInformation'
    { _goiConsistencyLevel = Nothing
    , _goiDirectoryARN = pDirectoryARN_
    , _goiObjectReference = pObjectReference_
    }


-- | The consistency level at which to retrieve the object information.
goiConsistencyLevel :: Lens' GetObjectInformation (Maybe ConsistencyLevel)
goiConsistencyLevel = lens _goiConsistencyLevel (\ s a -> s{_goiConsistencyLevel = a})

-- | The ARN of the directory being retrieved.
goiDirectoryARN :: Lens' GetObjectInformation Text
goiDirectoryARN = lens _goiDirectoryARN (\ s a -> s{_goiDirectoryARN = a})

-- | A reference to the object.
goiObjectReference :: Lens' GetObjectInformation ObjectReference
goiObjectReference = lens _goiObjectReference (\ s a -> s{_goiObjectReference = a})

instance AWSRequest GetObjectInformation where
        type Rs GetObjectInformation =
             GetObjectInformationResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 GetObjectInformationResponse' <$>
                   (x .?> "ObjectIdentifier") <*>
                     (x .?> "SchemaFacets" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetObjectInformation where

instance NFData GetObjectInformation where

instance ToHeaders GetObjectInformation where
        toHeaders GetObjectInformation'{..}
          = mconcat
              ["x-amz-consistency-level" =# _goiConsistencyLevel,
               "x-amz-data-partition" =# _goiDirectoryARN]

instance ToJSON GetObjectInformation where
        toJSON GetObjectInformation'{..}
          = object
              (catMaybes
                 [Just ("ObjectReference" .= _goiObjectReference)])

instance ToPath GetObjectInformation where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/object/information"

instance ToQuery GetObjectInformation where
        toQuery = const mempty

-- | /See:/ 'getObjectInformationResponse' smart constructor.
data GetObjectInformationResponse = GetObjectInformationResponse'
  { _goirsObjectIdentifier :: !(Maybe Text)
  , _goirsSchemaFacets     :: !(Maybe [SchemaFacet])
  , _goirsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetObjectInformationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goirsObjectIdentifier' - The @ObjectIdentifier@ of the specified object.
--
-- * 'goirsSchemaFacets' - The facets attached to the specified object. Although the response does not include minor version information, the most recently applied minor version of each Facet is in effect. See 'GetAppliedSchemaVersion' for details.
--
-- * 'goirsResponseStatus' - -- | The response status code.
getObjectInformationResponse
    :: Int -- ^ 'goirsResponseStatus'
    -> GetObjectInformationResponse
getObjectInformationResponse pResponseStatus_ =
  GetObjectInformationResponse'
    { _goirsObjectIdentifier = Nothing
    , _goirsSchemaFacets = Nothing
    , _goirsResponseStatus = pResponseStatus_
    }


-- | The @ObjectIdentifier@ of the specified object.
goirsObjectIdentifier :: Lens' GetObjectInformationResponse (Maybe Text)
goirsObjectIdentifier = lens _goirsObjectIdentifier (\ s a -> s{_goirsObjectIdentifier = a})

-- | The facets attached to the specified object. Although the response does not include minor version information, the most recently applied minor version of each Facet is in effect. See 'GetAppliedSchemaVersion' for details.
goirsSchemaFacets :: Lens' GetObjectInformationResponse [SchemaFacet]
goirsSchemaFacets = lens _goirsSchemaFacets (\ s a -> s{_goirsSchemaFacets = a}) . _Default . _Coerce

-- | -- | The response status code.
goirsResponseStatus :: Lens' GetObjectInformationResponse Int
goirsResponseStatus = lens _goirsResponseStatus (\ s a -> s{_goirsResponseStatus = a})

instance NFData GetObjectInformationResponse where
