{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectPoliciesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectPoliciesResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'ListObjectPolicies' response operation.
--
--
--
-- /See:/ 'batchListObjectPoliciesResponse' smart constructor.
data BatchListObjectPoliciesResponse = BatchListObjectPoliciesResponse'
  { _blopsNextToken ::
      !(Maybe Text),
    _blopsAttachedPolicyIds ::
      !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchListObjectPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blopsNextToken' - The pagination token.
--
-- * 'blopsAttachedPolicyIds' - A list of policy @ObjectIdentifiers@ , that are attached to the object.
batchListObjectPoliciesResponse ::
  BatchListObjectPoliciesResponse
batchListObjectPoliciesResponse =
  BatchListObjectPoliciesResponse'
    { _blopsNextToken = Nothing,
      _blopsAttachedPolicyIds = Nothing
    }

-- | The pagination token.
blopsNextToken :: Lens' BatchListObjectPoliciesResponse (Maybe Text)
blopsNextToken = lens _blopsNextToken (\s a -> s {_blopsNextToken = a})

-- | A list of policy @ObjectIdentifiers@ , that are attached to the object.
blopsAttachedPolicyIds :: Lens' BatchListObjectPoliciesResponse [Text]
blopsAttachedPolicyIds = lens _blopsAttachedPolicyIds (\s a -> s {_blopsAttachedPolicyIds = a}) . _Default . _Coerce

instance FromJSON BatchListObjectPoliciesResponse where
  parseJSON =
    withObject
      "BatchListObjectPoliciesResponse"
      ( \x ->
          BatchListObjectPoliciesResponse'
            <$> (x .:? "NextToken") <*> (x .:? "AttachedPolicyIds" .!= mempty)
      )

instance Hashable BatchListObjectPoliciesResponse

instance NFData BatchListObjectPoliciesResponse
