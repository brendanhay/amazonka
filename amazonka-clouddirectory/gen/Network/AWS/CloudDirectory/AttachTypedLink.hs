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
-- Module      : Network.AWS.CloudDirectory.AttachTypedLink
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a typed link to a specified source and target object. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
--
--
module Network.AWS.CloudDirectory.AttachTypedLink
    (
    -- * Creating a Request
      attachTypedLink
    , AttachTypedLink
    -- * Request Lenses
    , atlDirectoryARN
    , atlSourceObjectReference
    , atlTargetObjectReference
    , atlTypedLinkFacet
    , atlAttributes

    -- * Destructuring the Response
    , attachTypedLinkResponse
    , AttachTypedLinkResponse
    -- * Response Lenses
    , atlrsTypedLinkSpecifier
    , atlrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'attachTypedLink' smart constructor.
data AttachTypedLink = AttachTypedLink'
  { _atlDirectoryARN          :: !Text
  , _atlSourceObjectReference :: !ObjectReference
  , _atlTargetObjectReference :: !ObjectReference
  , _atlTypedLinkFacet        :: !TypedLinkSchemaAndFacetName
  , _atlAttributes            :: ![AttributeNameAndValue]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachTypedLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atlDirectoryARN' - The Amazon Resource Name (ARN) of the directory where you want to attach the typed link.
--
-- * 'atlSourceObjectReference' - Identifies the source object that the typed link will attach to.
--
-- * 'atlTargetObjectReference' - Identifies the target object that the typed link will attach to.
--
-- * 'atlTypedLinkFacet' - Identifies the typed link facet that is associated with the typed link.
--
-- * 'atlAttributes' - A set of attributes that are associated with the typed link.
attachTypedLink
    :: Text -- ^ 'atlDirectoryARN'
    -> ObjectReference -- ^ 'atlSourceObjectReference'
    -> ObjectReference -- ^ 'atlTargetObjectReference'
    -> TypedLinkSchemaAndFacetName -- ^ 'atlTypedLinkFacet'
    -> AttachTypedLink
attachTypedLink pDirectoryARN_ pSourceObjectReference_ pTargetObjectReference_ pTypedLinkFacet_ =
  AttachTypedLink'
    { _atlDirectoryARN = pDirectoryARN_
    , _atlSourceObjectReference = pSourceObjectReference_
    , _atlTargetObjectReference = pTargetObjectReference_
    , _atlTypedLinkFacet = pTypedLinkFacet_
    , _atlAttributes = mempty
    }


-- | The Amazon Resource Name (ARN) of the directory where you want to attach the typed link.
atlDirectoryARN :: Lens' AttachTypedLink Text
atlDirectoryARN = lens _atlDirectoryARN (\ s a -> s{_atlDirectoryARN = a})

-- | Identifies the source object that the typed link will attach to.
atlSourceObjectReference :: Lens' AttachTypedLink ObjectReference
atlSourceObjectReference = lens _atlSourceObjectReference (\ s a -> s{_atlSourceObjectReference = a})

-- | Identifies the target object that the typed link will attach to.
atlTargetObjectReference :: Lens' AttachTypedLink ObjectReference
atlTargetObjectReference = lens _atlTargetObjectReference (\ s a -> s{_atlTargetObjectReference = a})

-- | Identifies the typed link facet that is associated with the typed link.
atlTypedLinkFacet :: Lens' AttachTypedLink TypedLinkSchemaAndFacetName
atlTypedLinkFacet = lens _atlTypedLinkFacet (\ s a -> s{_atlTypedLinkFacet = a})

-- | A set of attributes that are associated with the typed link.
atlAttributes :: Lens' AttachTypedLink [AttributeNameAndValue]
atlAttributes = lens _atlAttributes (\ s a -> s{_atlAttributes = a}) . _Coerce

instance AWSRequest AttachTypedLink where
        type Rs AttachTypedLink = AttachTypedLinkResponse
        request = putJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 AttachTypedLinkResponse' <$>
                   (x .?> "TypedLinkSpecifier") <*> (pure (fromEnum s)))

instance Hashable AttachTypedLink where

instance NFData AttachTypedLink where

instance ToHeaders AttachTypedLink where
        toHeaders AttachTypedLink'{..}
          = mconcat
              ["x-amz-data-partition" =# _atlDirectoryARN]

instance ToJSON AttachTypedLink where
        toJSON AttachTypedLink'{..}
          = object
              (catMaybes
                 [Just
                    ("SourceObjectReference" .=
                       _atlSourceObjectReference),
                  Just
                    ("TargetObjectReference" .=
                       _atlTargetObjectReference),
                  Just ("TypedLinkFacet" .= _atlTypedLinkFacet),
                  Just ("Attributes" .= _atlAttributes)])

instance ToPath AttachTypedLink where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/typedlink/attach"

instance ToQuery AttachTypedLink where
        toQuery = const mempty

-- | /See:/ 'attachTypedLinkResponse' smart constructor.
data AttachTypedLinkResponse = AttachTypedLinkResponse'
  { _atlrsTypedLinkSpecifier :: !(Maybe TypedLinkSpecifier)
  , _atlrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachTypedLinkResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atlrsTypedLinkSpecifier' - Returns a typed link specifier as output.
--
-- * 'atlrsResponseStatus' - -- | The response status code.
attachTypedLinkResponse
    :: Int -- ^ 'atlrsResponseStatus'
    -> AttachTypedLinkResponse
attachTypedLinkResponse pResponseStatus_ =
  AttachTypedLinkResponse'
    { _atlrsTypedLinkSpecifier = Nothing
    , _atlrsResponseStatus = pResponseStatus_
    }


-- | Returns a typed link specifier as output.
atlrsTypedLinkSpecifier :: Lens' AttachTypedLinkResponse (Maybe TypedLinkSpecifier)
atlrsTypedLinkSpecifier = lens _atlrsTypedLinkSpecifier (\ s a -> s{_atlrsTypedLinkSpecifier = a})

-- | -- | The response status code.
atlrsResponseStatus :: Lens' AttachTypedLinkResponse Int
atlrsResponseStatus = lens _atlrsResponseStatus (\ s a -> s{_atlrsResponseStatus = a})

instance NFData AttachTypedLinkResponse where
