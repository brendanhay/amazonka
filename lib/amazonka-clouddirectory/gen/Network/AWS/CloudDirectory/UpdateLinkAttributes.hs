{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.UpdateLinkAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a given typed link’s attributes. Attributes to be updated must not contribute to the typed link’s identity, as defined by its @IdentityAttributeOrder@ .
module Network.AWS.CloudDirectory.UpdateLinkAttributes
  ( -- * Creating a Request
    updateLinkAttributes,
    UpdateLinkAttributes,

    -- * Request Lenses
    ulaDirectoryARN,
    ulaTypedLinkSpecifier,
    ulaAttributeUpdates,

    -- * Destructuring the Response
    updateLinkAttributesResponse,
    UpdateLinkAttributesResponse,

    -- * Response Lenses
    ularsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateLinkAttributes' smart constructor.
data UpdateLinkAttributes = UpdateLinkAttributes'
  { _ulaDirectoryARN ::
      !Text,
    _ulaTypedLinkSpecifier :: !TypedLinkSpecifier,
    _ulaAttributeUpdates :: ![LinkAttributeUpdate]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateLinkAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ulaDirectoryARN' - The Amazon Resource Name (ARN) that is associated with the Directory where the updated typed link resides. For more information, see 'arns' or <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- * 'ulaTypedLinkSpecifier' - Allows a typed link specifier to be accepted as input.
--
-- * 'ulaAttributeUpdates' - The attributes update structure.
updateLinkAttributes ::
  -- | 'ulaDirectoryARN'
  Text ->
  -- | 'ulaTypedLinkSpecifier'
  TypedLinkSpecifier ->
  UpdateLinkAttributes
updateLinkAttributes pDirectoryARN_ pTypedLinkSpecifier_ =
  UpdateLinkAttributes'
    { _ulaDirectoryARN = pDirectoryARN_,
      _ulaTypedLinkSpecifier = pTypedLinkSpecifier_,
      _ulaAttributeUpdates = mempty
    }

-- | The Amazon Resource Name (ARN) that is associated with the Directory where the updated typed link resides. For more information, see 'arns' or <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
ulaDirectoryARN :: Lens' UpdateLinkAttributes Text
ulaDirectoryARN = lens _ulaDirectoryARN (\s a -> s {_ulaDirectoryARN = a})

-- | Allows a typed link specifier to be accepted as input.
ulaTypedLinkSpecifier :: Lens' UpdateLinkAttributes TypedLinkSpecifier
ulaTypedLinkSpecifier = lens _ulaTypedLinkSpecifier (\s a -> s {_ulaTypedLinkSpecifier = a})

-- | The attributes update structure.
ulaAttributeUpdates :: Lens' UpdateLinkAttributes [LinkAttributeUpdate]
ulaAttributeUpdates = lens _ulaAttributeUpdates (\s a -> s {_ulaAttributeUpdates = a}) . _Coerce

instance AWSRequest UpdateLinkAttributes where
  type Rs UpdateLinkAttributes = UpdateLinkAttributesResponse
  request = postJSON cloudDirectory
  response =
    receiveEmpty
      (\s h x -> UpdateLinkAttributesResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateLinkAttributes

instance NFData UpdateLinkAttributes

instance ToHeaders UpdateLinkAttributes where
  toHeaders UpdateLinkAttributes' {..} =
    mconcat ["x-amz-data-partition" =# _ulaDirectoryARN]

instance ToJSON UpdateLinkAttributes where
  toJSON UpdateLinkAttributes' {..} =
    object
      ( catMaybes
          [ Just ("TypedLinkSpecifier" .= _ulaTypedLinkSpecifier),
            Just ("AttributeUpdates" .= _ulaAttributeUpdates)
          ]
      )

instance ToPath UpdateLinkAttributes where
  toPath =
    const
      "/amazonclouddirectory/2017-01-11/typedlink/attributes/update"

instance ToQuery UpdateLinkAttributes where
  toQuery = const mempty

-- | /See:/ 'updateLinkAttributesResponse' smart constructor.
newtype UpdateLinkAttributesResponse = UpdateLinkAttributesResponse'
  { _ularsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateLinkAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ularsResponseStatus' - -- | The response status code.
updateLinkAttributesResponse ::
  -- | 'ularsResponseStatus'
  Int ->
  UpdateLinkAttributesResponse
updateLinkAttributesResponse pResponseStatus_ =
  UpdateLinkAttributesResponse'
    { _ularsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
ularsResponseStatus :: Lens' UpdateLinkAttributesResponse Int
ularsResponseStatus = lens _ularsResponseStatus (\s a -> s {_ularsResponseStatus = a})

instance NFData UpdateLinkAttributesResponse
