{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinks where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.TypedLinkAttributeRange
import Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns a paginated list of all the outgoing 'TypedLinkSpecifier' information for an object inside a 'BatchRead' operation. For more information, see 'ListOutgoingTypedLinks' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchListOutgoingTypedLinks' smart constructor.
data BatchListOutgoingTypedLinks = BatchListOutgoingTypedLinks'
  { _blotlsFilterAttributeRanges ::
      !(Maybe [TypedLinkAttributeRange]),
    _blotlsNextToken :: !(Maybe Text),
    _blotlsFilterTypedLink ::
      !( Maybe
           TypedLinkSchemaAndFacetName
       ),
    _blotlsMaxResults :: !(Maybe Nat),
    _blotlsObjectReference ::
      !ObjectReference
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchListOutgoingTypedLinks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blotlsFilterAttributeRanges' - Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
--
-- * 'blotlsNextToken' - The pagination token.
--
-- * 'blotlsFilterTypedLink' - Filters are interpreted in the order of the attributes defined on the typed link facet, not the order they are supplied to any API calls.
--
-- * 'blotlsMaxResults' - The maximum number of results to retrieve.
--
-- * 'blotlsObjectReference' - The reference that identifies the object whose attributes will be listed.
batchListOutgoingTypedLinks ::
  -- | 'blotlsObjectReference'
  ObjectReference ->
  BatchListOutgoingTypedLinks
batchListOutgoingTypedLinks pObjectReference_ =
  BatchListOutgoingTypedLinks'
    { _blotlsFilterAttributeRanges =
        Nothing,
      _blotlsNextToken = Nothing,
      _blotlsFilterTypedLink = Nothing,
      _blotlsMaxResults = Nothing,
      _blotlsObjectReference = pObjectReference_
    }

-- | Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
blotlsFilterAttributeRanges :: Lens' BatchListOutgoingTypedLinks [TypedLinkAttributeRange]
blotlsFilterAttributeRanges = lens _blotlsFilterAttributeRanges (\s a -> s {_blotlsFilterAttributeRanges = a}) . _Default . _Coerce

-- | The pagination token.
blotlsNextToken :: Lens' BatchListOutgoingTypedLinks (Maybe Text)
blotlsNextToken = lens _blotlsNextToken (\s a -> s {_blotlsNextToken = a})

-- | Filters are interpreted in the order of the attributes defined on the typed link facet, not the order they are supplied to any API calls.
blotlsFilterTypedLink :: Lens' BatchListOutgoingTypedLinks (Maybe TypedLinkSchemaAndFacetName)
blotlsFilterTypedLink = lens _blotlsFilterTypedLink (\s a -> s {_blotlsFilterTypedLink = a})

-- | The maximum number of results to retrieve.
blotlsMaxResults :: Lens' BatchListOutgoingTypedLinks (Maybe Natural)
blotlsMaxResults = lens _blotlsMaxResults (\s a -> s {_blotlsMaxResults = a}) . mapping _Nat

-- | The reference that identifies the object whose attributes will be listed.
blotlsObjectReference :: Lens' BatchListOutgoingTypedLinks ObjectReference
blotlsObjectReference = lens _blotlsObjectReference (\s a -> s {_blotlsObjectReference = a})

instance Hashable BatchListOutgoingTypedLinks

instance NFData BatchListOutgoingTypedLinks

instance ToJSON BatchListOutgoingTypedLinks where
  toJSON BatchListOutgoingTypedLinks' {..} =
    object
      ( catMaybes
          [ ("FilterAttributeRanges" .=) <$> _blotlsFilterAttributeRanges,
            ("NextToken" .=) <$> _blotlsNextToken,
            ("FilterTypedLink" .=) <$> _blotlsFilterTypedLink,
            ("MaxResults" .=) <$> _blotlsMaxResults,
            Just ("ObjectReference" .= _blotlsObjectReference)
          ]
      )
