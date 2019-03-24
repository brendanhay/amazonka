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
-- Module      : Network.AWS.CloudDirectory.GetLinkAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves attributes that are associated with a typed link.
--
--
module Network.AWS.CloudDirectory.GetLinkAttributes
    (
    -- * Creating a Request
      getLinkAttributes
    , GetLinkAttributes
    -- * Request Lenses
    , glaConsistencyLevel
    , glaDirectoryARN
    , glaTypedLinkSpecifier
    , glaAttributeNames

    -- * Destructuring the Response
    , getLinkAttributesResponse
    , GetLinkAttributesResponse
    -- * Response Lenses
    , glarsAttributes
    , glarsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getLinkAttributes' smart constructor.
data GetLinkAttributes = GetLinkAttributes'
  { _glaConsistencyLevel   :: !(Maybe ConsistencyLevel)
  , _glaDirectoryARN       :: !Text
  , _glaTypedLinkSpecifier :: !TypedLinkSpecifier
  , _glaAttributeNames     :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLinkAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glaConsistencyLevel' - The consistency level at which to retrieve the attributes on a typed link.
--
-- * 'glaDirectoryARN' - The Amazon Resource Name (ARN) that is associated with the Directory where the typed link resides. For more information, see 'arns' or <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- * 'glaTypedLinkSpecifier' - Allows a typed link specifier to be accepted as input.
--
-- * 'glaAttributeNames' - A list of attribute names whose values will be retrieved.
getLinkAttributes
    :: Text -- ^ 'glaDirectoryARN'
    -> TypedLinkSpecifier -- ^ 'glaTypedLinkSpecifier'
    -> GetLinkAttributes
getLinkAttributes pDirectoryARN_ pTypedLinkSpecifier_ =
  GetLinkAttributes'
    { _glaConsistencyLevel = Nothing
    , _glaDirectoryARN = pDirectoryARN_
    , _glaTypedLinkSpecifier = pTypedLinkSpecifier_
    , _glaAttributeNames = mempty
    }


-- | The consistency level at which to retrieve the attributes on a typed link.
glaConsistencyLevel :: Lens' GetLinkAttributes (Maybe ConsistencyLevel)
glaConsistencyLevel = lens _glaConsistencyLevel (\ s a -> s{_glaConsistencyLevel = a})

-- | The Amazon Resource Name (ARN) that is associated with the Directory where the typed link resides. For more information, see 'arns' or <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
glaDirectoryARN :: Lens' GetLinkAttributes Text
glaDirectoryARN = lens _glaDirectoryARN (\ s a -> s{_glaDirectoryARN = a})

-- | Allows a typed link specifier to be accepted as input.
glaTypedLinkSpecifier :: Lens' GetLinkAttributes TypedLinkSpecifier
glaTypedLinkSpecifier = lens _glaTypedLinkSpecifier (\ s a -> s{_glaTypedLinkSpecifier = a})

-- | A list of attribute names whose values will be retrieved.
glaAttributeNames :: Lens' GetLinkAttributes [Text]
glaAttributeNames = lens _glaAttributeNames (\ s a -> s{_glaAttributeNames = a}) . _Coerce

instance AWSRequest GetLinkAttributes where
        type Rs GetLinkAttributes = GetLinkAttributesResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 GetLinkAttributesResponse' <$>
                   (x .?> "Attributes" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable GetLinkAttributes where

instance NFData GetLinkAttributes where

instance ToHeaders GetLinkAttributes where
        toHeaders GetLinkAttributes'{..}
          = mconcat
              ["x-amz-data-partition" =# _glaDirectoryARN]

instance ToJSON GetLinkAttributes where
        toJSON GetLinkAttributes'{..}
          = object
              (catMaybes
                 [("ConsistencyLevel" .=) <$> _glaConsistencyLevel,
                  Just
                    ("TypedLinkSpecifier" .= _glaTypedLinkSpecifier),
                  Just ("AttributeNames" .= _glaAttributeNames)])

instance ToPath GetLinkAttributes where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/typedlink/attributes/get"

instance ToQuery GetLinkAttributes where
        toQuery = const mempty

-- | /See:/ 'getLinkAttributesResponse' smart constructor.
data GetLinkAttributesResponse = GetLinkAttributesResponse'
  { _glarsAttributes     :: !(Maybe [AttributeKeyAndValue])
  , _glarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLinkAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glarsAttributes' - The attributes that are associated with the typed link.
--
-- * 'glarsResponseStatus' - -- | The response status code.
getLinkAttributesResponse
    :: Int -- ^ 'glarsResponseStatus'
    -> GetLinkAttributesResponse
getLinkAttributesResponse pResponseStatus_ =
  GetLinkAttributesResponse'
    {_glarsAttributes = Nothing, _glarsResponseStatus = pResponseStatus_}


-- | The attributes that are associated with the typed link.
glarsAttributes :: Lens' GetLinkAttributesResponse [AttributeKeyAndValue]
glarsAttributes = lens _glarsAttributes (\ s a -> s{_glarsAttributes = a}) . _Default . _Coerce

-- | -- | The response status code.
glarsResponseStatus :: Lens' GetLinkAttributesResponse Int
glarsResponseStatus = lens _glarsResponseStatus (\ s a -> s{_glarsResponseStatus = a})

instance NFData GetLinkAttributesResponse where
