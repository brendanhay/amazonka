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
-- Module      : Network.AWS.CloudDirectory.DetachTypedLink
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a typed link from a specified source and target object. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
--
--
module Network.AWS.CloudDirectory.DetachTypedLink
    (
    -- * Creating a Request
      detachTypedLink
    , DetachTypedLink
    -- * Request Lenses
    , dtlDirectoryARN
    , dtlTypedLinkSpecifier

    -- * Destructuring the Response
    , detachTypedLinkResponse
    , DetachTypedLinkResponse
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detachTypedLink' smart constructor.
data DetachTypedLink = DetachTypedLink'
  { _dtlDirectoryARN       :: !Text
  , _dtlTypedLinkSpecifier :: !TypedLinkSpecifier
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachTypedLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtlDirectoryARN' - The Amazon Resource Name (ARN) of the directory where you want to detach the typed link.
--
-- * 'dtlTypedLinkSpecifier' - Used to accept a typed link specifier as input.
detachTypedLink
    :: Text -- ^ 'dtlDirectoryARN'
    -> TypedLinkSpecifier -- ^ 'dtlTypedLinkSpecifier'
    -> DetachTypedLink
detachTypedLink pDirectoryARN_ pTypedLinkSpecifier_ =
  DetachTypedLink'
    { _dtlDirectoryARN = pDirectoryARN_
    , _dtlTypedLinkSpecifier = pTypedLinkSpecifier_
    }


-- | The Amazon Resource Name (ARN) of the directory where you want to detach the typed link.
dtlDirectoryARN :: Lens' DetachTypedLink Text
dtlDirectoryARN = lens _dtlDirectoryARN (\ s a -> s{_dtlDirectoryARN = a})

-- | Used to accept a typed link specifier as input.
dtlTypedLinkSpecifier :: Lens' DetachTypedLink TypedLinkSpecifier
dtlTypedLinkSpecifier = lens _dtlTypedLinkSpecifier (\ s a -> s{_dtlTypedLinkSpecifier = a})

instance AWSRequest DetachTypedLink where
        type Rs DetachTypedLink = DetachTypedLinkResponse
        request = putJSON cloudDirectory
        response = receiveNull DetachTypedLinkResponse'

instance Hashable DetachTypedLink where

instance NFData DetachTypedLink where

instance ToHeaders DetachTypedLink where
        toHeaders DetachTypedLink'{..}
          = mconcat
              ["x-amz-data-partition" =# _dtlDirectoryARN]

instance ToJSON DetachTypedLink where
        toJSON DetachTypedLink'{..}
          = object
              (catMaybes
                 [Just
                    ("TypedLinkSpecifier" .= _dtlTypedLinkSpecifier)])

instance ToPath DetachTypedLink where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/typedlink/detach"

instance ToQuery DetachTypedLink where
        toQuery = const mempty

-- | /See:/ 'detachTypedLinkResponse' smart constructor.
data DetachTypedLinkResponse =
  DetachTypedLinkResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachTypedLinkResponse' with the minimum fields required to make a request.
--
detachTypedLinkResponse
    :: DetachTypedLinkResponse
detachTypedLinkResponse = DetachTypedLinkResponse'


instance NFData DetachTypedLinkResponse where
