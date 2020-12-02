{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Condition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Condition where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | A container for describing a condition that must be met for the specified redirect to apply. For example, 1. If request is for pages in the @/docs@ folder, redirect to the @/documents@ folder. 2. If request results in HTTP error 4xx, redirect request to another host where you might process the error.
--
--
--
-- /See:/ 'condition' smart constructor.
data Condition = Condition'
  { _cKeyPrefixEquals :: !(Maybe Text),
    _cHTTPErrorCodeReturnedEquals :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Condition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cKeyPrefixEquals' - The object key name prefix when the redirect is applied. For example, to redirect requests for @ExamplePage.html@ , the key prefix will be @ExamplePage.html@ . To redirect request for all pages with the prefix @docs/@ , the key prefix will be @/docs@ , which identifies all objects in the @docs/@ folder. Required when the parent element @Condition@ is specified and sibling @HttpErrorCodeReturnedEquals@ is not specified. If both conditions are specified, both must be true for the redirect to be applied.
--
-- * 'cHTTPErrorCodeReturnedEquals' - The HTTP error code when the redirect is applied. In the event of an error, if the error code equals this value, then the specified redirect is applied. Required when parent element @Condition@ is specified and sibling @KeyPrefixEquals@ is not specified. If both are specified, then both must be true for the redirect to be applied.
condition ::
  Condition
condition =
  Condition'
    { _cKeyPrefixEquals = Nothing,
      _cHTTPErrorCodeReturnedEquals = Nothing
    }

-- | The object key name prefix when the redirect is applied. For example, to redirect requests for @ExamplePage.html@ , the key prefix will be @ExamplePage.html@ . To redirect request for all pages with the prefix @docs/@ , the key prefix will be @/docs@ , which identifies all objects in the @docs/@ folder. Required when the parent element @Condition@ is specified and sibling @HttpErrorCodeReturnedEquals@ is not specified. If both conditions are specified, both must be true for the redirect to be applied.
cKeyPrefixEquals :: Lens' Condition (Maybe Text)
cKeyPrefixEquals = lens _cKeyPrefixEquals (\s a -> s {_cKeyPrefixEquals = a})

-- | The HTTP error code when the redirect is applied. In the event of an error, if the error code equals this value, then the specified redirect is applied. Required when parent element @Condition@ is specified and sibling @KeyPrefixEquals@ is not specified. If both are specified, then both must be true for the redirect to be applied.
cHTTPErrorCodeReturnedEquals :: Lens' Condition (Maybe Text)
cHTTPErrorCodeReturnedEquals = lens _cHTTPErrorCodeReturnedEquals (\s a -> s {_cHTTPErrorCodeReturnedEquals = a})

instance FromXML Condition where
  parseXML x =
    Condition'
      <$> (x .@? "KeyPrefixEquals") <*> (x .@? "HttpErrorCodeReturnedEquals")

instance Hashable Condition

instance NFData Condition

instance ToXML Condition where
  toXML Condition' {..} =
    mconcat
      [ "KeyPrefixEquals" @= _cKeyPrefixEquals,
        "HttpErrorCodeReturnedEquals" @= _cHTTPErrorCodeReturnedEquals
      ]
