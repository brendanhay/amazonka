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
-- Module      : Network.AWS.IoT.UpdateAuditSuppression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Device Defender audit suppression.
module Network.AWS.IoT.UpdateAuditSuppression
  ( -- * Creating a Request
    updateAuditSuppression,
    UpdateAuditSuppression,

    -- * Request Lenses
    uasExpirationDate,
    uasSuppressIndefinitely,
    uasDescription,
    uasCheckName,
    uasResourceIdentifier,

    -- * Destructuring the Response
    updateAuditSuppressionResponse,
    UpdateAuditSuppressionResponse,

    -- * Response Lenses
    uasrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateAuditSuppression' smart constructor.
data UpdateAuditSuppression = UpdateAuditSuppression'
  { _uasExpirationDate ::
      !(Maybe POSIX),
    _uasSuppressIndefinitely :: !(Maybe Bool),
    _uasDescription :: !(Maybe Text),
    _uasCheckName :: !Text,
    _uasResourceIdentifier :: !ResourceIdentifier
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateAuditSuppression' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uasExpirationDate' - The expiration date (epoch timestamp in seconds) that you want the suppression to adhere to.
--
-- * 'uasSuppressIndefinitely' - Indicates whether a suppression should exist indefinitely or not.
--
-- * 'uasDescription' - The description of the audit suppression.
--
-- * 'uasCheckName' - Undocumented member.
--
-- * 'uasResourceIdentifier' - Undocumented member.
updateAuditSuppression ::
  -- | 'uasCheckName'
  Text ->
  -- | 'uasResourceIdentifier'
  ResourceIdentifier ->
  UpdateAuditSuppression
updateAuditSuppression pCheckName_ pResourceIdentifier_ =
  UpdateAuditSuppression'
    { _uasExpirationDate = Nothing,
      _uasSuppressIndefinitely = Nothing,
      _uasDescription = Nothing,
      _uasCheckName = pCheckName_,
      _uasResourceIdentifier = pResourceIdentifier_
    }

-- | The expiration date (epoch timestamp in seconds) that you want the suppression to adhere to.
uasExpirationDate :: Lens' UpdateAuditSuppression (Maybe UTCTime)
uasExpirationDate = lens _uasExpirationDate (\s a -> s {_uasExpirationDate = a}) . mapping _Time

-- | Indicates whether a suppression should exist indefinitely or not.
uasSuppressIndefinitely :: Lens' UpdateAuditSuppression (Maybe Bool)
uasSuppressIndefinitely = lens _uasSuppressIndefinitely (\s a -> s {_uasSuppressIndefinitely = a})

-- | The description of the audit suppression.
uasDescription :: Lens' UpdateAuditSuppression (Maybe Text)
uasDescription = lens _uasDescription (\s a -> s {_uasDescription = a})

-- | Undocumented member.
uasCheckName :: Lens' UpdateAuditSuppression Text
uasCheckName = lens _uasCheckName (\s a -> s {_uasCheckName = a})

-- | Undocumented member.
uasResourceIdentifier :: Lens' UpdateAuditSuppression ResourceIdentifier
uasResourceIdentifier = lens _uasResourceIdentifier (\s a -> s {_uasResourceIdentifier = a})

instance AWSRequest UpdateAuditSuppression where
  type Rs UpdateAuditSuppression = UpdateAuditSuppressionResponse
  request = patchJSON ioT
  response =
    receiveEmpty
      ( \s h x ->
          UpdateAuditSuppressionResponse' <$> (pure (fromEnum s))
      )

instance Hashable UpdateAuditSuppression

instance NFData UpdateAuditSuppression

instance ToHeaders UpdateAuditSuppression where
  toHeaders = const mempty

instance ToJSON UpdateAuditSuppression where
  toJSON UpdateAuditSuppression' {..} =
    object
      ( catMaybes
          [ ("expirationDate" .=) <$> _uasExpirationDate,
            ("suppressIndefinitely" .=) <$> _uasSuppressIndefinitely,
            ("description" .=) <$> _uasDescription,
            Just ("checkName" .= _uasCheckName),
            Just ("resourceIdentifier" .= _uasResourceIdentifier)
          ]
      )

instance ToPath UpdateAuditSuppression where
  toPath = const "/audit/suppressions/update"

instance ToQuery UpdateAuditSuppression where
  toQuery = const mempty

-- | /See:/ 'updateAuditSuppressionResponse' smart constructor.
newtype UpdateAuditSuppressionResponse = UpdateAuditSuppressionResponse'
  { _uasrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateAuditSuppressionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uasrsResponseStatus' - -- | The response status code.
updateAuditSuppressionResponse ::
  -- | 'uasrsResponseStatus'
  Int ->
  UpdateAuditSuppressionResponse
updateAuditSuppressionResponse pResponseStatus_ =
  UpdateAuditSuppressionResponse'
    { _uasrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
uasrsResponseStatus :: Lens' UpdateAuditSuppressionResponse Int
uasrsResponseStatus = lens _uasrsResponseStatus (\s a -> s {_uasrsResponseStatus = a})

instance NFData UpdateAuditSuppressionResponse
