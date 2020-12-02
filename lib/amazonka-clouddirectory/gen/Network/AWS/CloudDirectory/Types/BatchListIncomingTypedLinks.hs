{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListIncomingTypedLinks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListIncomingTypedLinks where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.TypedLinkAttributeRange
import Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns a paginated list of all the incoming 'TypedLinkSpecifier' information for an object inside a 'BatchRead' operation. For more information, see 'ListIncomingTypedLinks' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchListIncomingTypedLinks' smart constructor.
data BatchListIncomingTypedLinks = BatchListIncomingTypedLinks'
  { _blitlsFilterAttributeRanges ::
      !(Maybe [TypedLinkAttributeRange]),
    _blitlsNextToken :: !(Maybe Text),
    _blitlsFilterTypedLink ::
      !( Maybe
           TypedLinkSchemaAndFacetName
       ),
    _blitlsMaxResults :: !(Maybe Nat),
    _blitlsObjectReference ::
      !ObjectReference
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchListIncomingTypedLinks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blitlsFilterAttributeRanges' - Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
--
-- * 'blitlsNextToken' - The pagination token.
--
-- * 'blitlsFilterTypedLink' - Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls.
--
-- * 'blitlsMaxResults' - The maximum number of results to retrieve.
--
-- * 'blitlsObjectReference' - The reference that identifies the object whose attributes will be listed.
batchListIncomingTypedLinks ::
  -- | 'blitlsObjectReference'
  ObjectReference ->
  BatchListIncomingTypedLinks
batchListIncomingTypedLinks pObjectReference_ =
  BatchListIncomingTypedLinks'
    { _blitlsFilterAttributeRanges =
        Nothing,
      _blitlsNextToken = Nothing,
      _blitlsFilterTypedLink = Nothing,
      _blitlsMaxResults = Nothing,
      _blitlsObjectReference = pObjectReference_
    }

-- | Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
blitlsFilterAttributeRanges :: Lens' BatchListIncomingTypedLinks [TypedLinkAttributeRange]
blitlsFilterAttributeRanges = lens _blitlsFilterAttributeRanges (\s a -> s {_blitlsFilterAttributeRanges = a}) . _Default . _Coerce

-- | The pagination token.
blitlsNextToken :: Lens' BatchListIncomingTypedLinks (Maybe Text)
blitlsNextToken = lens _blitlsNextToken (\s a -> s {_blitlsNextToken = a})

-- | Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls.
blitlsFilterTypedLink :: Lens' BatchListIncomingTypedLinks (Maybe TypedLinkSchemaAndFacetName)
blitlsFilterTypedLink = lens _blitlsFilterTypedLink (\s a -> s {_blitlsFilterTypedLink = a})

-- | The maximum number of results to retrieve.
blitlsMaxResults :: Lens' BatchListIncomingTypedLinks (Maybe Natural)
blitlsMaxResults = lens _blitlsMaxResults (\s a -> s {_blitlsMaxResults = a}) . mapping _Nat

-- | The reference that identifies the object whose attributes will be listed.
blitlsObjectReference :: Lens' BatchListIncomingTypedLinks ObjectReference
blitlsObjectReference = lens _blitlsObjectReference (\s a -> s {_blitlsObjectReference = a})

instance Hashable BatchListIncomingTypedLinks

instance NFData BatchListIncomingTypedLinks

instance ToJSON BatchListIncomingTypedLinks where
  toJSON BatchListIncomingTypedLinks' {..} =
    object
      ( catMaybes
          [ ("FilterAttributeRanges" .=) <$> _blitlsFilterAttributeRanges,
            ("NextToken" .=) <$> _blitlsNextToken,
            ("FilterTypedLink" .=) <$> _blitlsFilterTypedLink,
            ("MaxResults" .=) <$> _blitlsMaxResults,
            Just ("ObjectReference" .= _blitlsObjectReference)
          ]
      )
