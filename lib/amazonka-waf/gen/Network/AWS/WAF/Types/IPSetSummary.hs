{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.IPSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.IPSetSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the identifier and the name of the @IPSet@ .
--
--
--
-- /See:/ 'ipSetSummary' smart constructor.
data IPSetSummary = IPSetSummary'
  { _issIPSetId :: !Text,
    _issName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IPSetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'issIPSetId' - The @IPSetId@ for an 'IPSet' . You can use @IPSetId@ in a 'GetIPSet' request to get detailed information about an 'IPSet' .
--
-- * 'issName' - A friendly name or description of the 'IPSet' . You can't change the name of an @IPSet@ after you create it.
ipSetSummary ::
  -- | 'issIPSetId'
  Text ->
  -- | 'issName'
  Text ->
  IPSetSummary
ipSetSummary pIPSetId_ pName_ =
  IPSetSummary' {_issIPSetId = pIPSetId_, _issName = pName_}

-- | The @IPSetId@ for an 'IPSet' . You can use @IPSetId@ in a 'GetIPSet' request to get detailed information about an 'IPSet' .
issIPSetId :: Lens' IPSetSummary Text
issIPSetId = lens _issIPSetId (\s a -> s {_issIPSetId = a})

-- | A friendly name or description of the 'IPSet' . You can't change the name of an @IPSet@ after you create it.
issName :: Lens' IPSetSummary Text
issName = lens _issName (\s a -> s {_issName = a})

instance FromJSON IPSetSummary where
  parseJSON =
    withObject
      "IPSetSummary"
      (\x -> IPSetSummary' <$> (x .: "IPSetId") <*> (x .: "Name"))

instance Hashable IPSetSummary

instance NFData IPSetSummary
