{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListPolicyAttachmentsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListPolicyAttachmentsResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'ListPolicyAttachments' response operation.
--
--
--
-- /See:/ 'batchListPolicyAttachmentsResponse' smart constructor.
data BatchListPolicyAttachmentsResponse = BatchListPolicyAttachmentsResponse'
  { _blpaObjectIdentifiers ::
      !(Maybe [Text]),
    _blpaNextToken ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchListPolicyAttachmentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blpaObjectIdentifiers' - A list of @ObjectIdentifiers@ to which the policy is attached.
--
-- * 'blpaNextToken' - The pagination token.
batchListPolicyAttachmentsResponse ::
  BatchListPolicyAttachmentsResponse
batchListPolicyAttachmentsResponse =
  BatchListPolicyAttachmentsResponse'
    { _blpaObjectIdentifiers =
        Nothing,
      _blpaNextToken = Nothing
    }

-- | A list of @ObjectIdentifiers@ to which the policy is attached.
blpaObjectIdentifiers :: Lens' BatchListPolicyAttachmentsResponse [Text]
blpaObjectIdentifiers = lens _blpaObjectIdentifiers (\s a -> s {_blpaObjectIdentifiers = a}) . _Default . _Coerce

-- | The pagination token.
blpaNextToken :: Lens' BatchListPolicyAttachmentsResponse (Maybe Text)
blpaNextToken = lens _blpaNextToken (\s a -> s {_blpaNextToken = a})

instance FromJSON BatchListPolicyAttachmentsResponse where
  parseJSON =
    withObject
      "BatchListPolicyAttachmentsResponse"
      ( \x ->
          BatchListPolicyAttachmentsResponse'
            <$> (x .:? "ObjectIdentifiers" .!= mempty) <*> (x .:? "NextToken")
      )

instance Hashable BatchListPolicyAttachmentsResponse

instance NFData BatchListPolicyAttachmentsResponse
