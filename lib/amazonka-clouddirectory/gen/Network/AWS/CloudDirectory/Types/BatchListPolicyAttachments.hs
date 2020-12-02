{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListPolicyAttachments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListPolicyAttachments where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns all of the @ObjectIdentifiers@ to which a given policy is attached inside a 'BatchRead' operation. For more information, see 'ListPolicyAttachments' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchListPolicyAttachments' smart constructor.
data BatchListPolicyAttachments = BatchListPolicyAttachments'
  { _blpasNextToken ::
      !(Maybe Text),
    _blpasMaxResults :: !(Maybe Nat),
    _blpasPolicyReference ::
      !ObjectReference
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchListPolicyAttachments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blpasNextToken' - The pagination token.
--
-- * 'blpasMaxResults' - The maximum number of results to retrieve.
--
-- * 'blpasPolicyReference' - The reference that identifies the policy object.
batchListPolicyAttachments ::
  -- | 'blpasPolicyReference'
  ObjectReference ->
  BatchListPolicyAttachments
batchListPolicyAttachments pPolicyReference_ =
  BatchListPolicyAttachments'
    { _blpasNextToken = Nothing,
      _blpasMaxResults = Nothing,
      _blpasPolicyReference = pPolicyReference_
    }

-- | The pagination token.
blpasNextToken :: Lens' BatchListPolicyAttachments (Maybe Text)
blpasNextToken = lens _blpasNextToken (\s a -> s {_blpasNextToken = a})

-- | The maximum number of results to retrieve.
blpasMaxResults :: Lens' BatchListPolicyAttachments (Maybe Natural)
blpasMaxResults = lens _blpasMaxResults (\s a -> s {_blpasMaxResults = a}) . mapping _Nat

-- | The reference that identifies the policy object.
blpasPolicyReference :: Lens' BatchListPolicyAttachments ObjectReference
blpasPolicyReference = lens _blpasPolicyReference (\s a -> s {_blpasPolicyReference = a})

instance Hashable BatchListPolicyAttachments

instance NFData BatchListPolicyAttachments

instance ToJSON BatchListPolicyAttachments where
  toJSON BatchListPolicyAttachments' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _blpasNextToken,
            ("MaxResults" .=) <$> _blpasMaxResults,
            Just ("PolicyReference" .= _blpasPolicyReference)
          ]
      )
