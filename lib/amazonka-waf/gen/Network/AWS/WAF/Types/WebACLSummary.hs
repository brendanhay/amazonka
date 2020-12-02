{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.WebACLSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.WebACLSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the identifier and the name or description of the 'WebACL' .
--
--
--
-- /See:/ 'webACLSummary' smart constructor.
data WebACLSummary = WebACLSummary'
  { _wasWebACLId :: !Text,
    _wasName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WebACLSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wasWebACLId' - A unique identifier for a @WebACL@ . You use @WebACLId@ to get information about a @WebACL@ (see 'GetWebACL' ), update a @WebACL@ (see 'UpdateWebACL' ), and delete a @WebACL@ from AWS WAF (see 'DeleteWebACL' ). @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
--
-- * 'wasName' - A friendly name or description of the 'WebACL' . You can't change the name of a @WebACL@ after you create it.
webACLSummary ::
  -- | 'wasWebACLId'
  Text ->
  -- | 'wasName'
  Text ->
  WebACLSummary
webACLSummary pWebACLId_ pName_ =
  WebACLSummary' {_wasWebACLId = pWebACLId_, _wasName = pName_}

-- | A unique identifier for a @WebACL@ . You use @WebACLId@ to get information about a @WebACL@ (see 'GetWebACL' ), update a @WebACL@ (see 'UpdateWebACL' ), and delete a @WebACL@ from AWS WAF (see 'DeleteWebACL' ). @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
wasWebACLId :: Lens' WebACLSummary Text
wasWebACLId = lens _wasWebACLId (\s a -> s {_wasWebACLId = a})

-- | A friendly name or description of the 'WebACL' . You can't change the name of a @WebACL@ after you create it.
wasName :: Lens' WebACLSummary Text
wasName = lens _wasName (\s a -> s {_wasName = a})

instance FromJSON WebACLSummary where
  parseJSON =
    withObject
      "WebACLSummary"
      (\x -> WebACLSummary' <$> (x .: "WebACLId") <*> (x .: "Name"))

instance Hashable WebACLSummary

instance NFData WebACLSummary
