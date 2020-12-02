{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchLookupPolicyResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchLookupPolicyResponse where

import Network.AWS.CloudDirectory.Types.PolicyToPath
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'LookupPolicy' response operation.
--
--
--
-- /See:/ 'batchLookupPolicyResponse' smart constructor.
data BatchLookupPolicyResponse = BatchLookupPolicyResponse'
  { _blpNextToken ::
      !(Maybe Text),
    _blpPolicyToPathList ::
      !(Maybe [PolicyToPath])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchLookupPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blpNextToken' - The pagination token.
--
-- * 'blpPolicyToPathList' - Provides list of path to policies. Policies contain @PolicyId@ , @ObjectIdentifier@ , and @PolicyType@ . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
batchLookupPolicyResponse ::
  BatchLookupPolicyResponse
batchLookupPolicyResponse =
  BatchLookupPolicyResponse'
    { _blpNextToken = Nothing,
      _blpPolicyToPathList = Nothing
    }

-- | The pagination token.
blpNextToken :: Lens' BatchLookupPolicyResponse (Maybe Text)
blpNextToken = lens _blpNextToken (\s a -> s {_blpNextToken = a})

-- | Provides list of path to policies. Policies contain @PolicyId@ , @ObjectIdentifier@ , and @PolicyType@ . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
blpPolicyToPathList :: Lens' BatchLookupPolicyResponse [PolicyToPath]
blpPolicyToPathList = lens _blpPolicyToPathList (\s a -> s {_blpPolicyToPathList = a}) . _Default . _Coerce

instance FromJSON BatchLookupPolicyResponse where
  parseJSON =
    withObject
      "BatchLookupPolicyResponse"
      ( \x ->
          BatchLookupPolicyResponse'
            <$> (x .:? "NextToken") <*> (x .:? "PolicyToPathList" .!= mempty)
      )

instance Hashable BatchLookupPolicyResponse

instance NFData BatchLookupPolicyResponse
