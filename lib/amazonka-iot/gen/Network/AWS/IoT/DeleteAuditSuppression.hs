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
-- Module      : Network.AWS.IoT.DeleteAuditSuppression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Device Defender audit suppression.
module Network.AWS.IoT.DeleteAuditSuppression
  ( -- * Creating a Request
    deleteAuditSuppression,
    DeleteAuditSuppression,

    -- * Request Lenses
    dasCheckName,
    dasResourceIdentifier,

    -- * Destructuring the Response
    deleteAuditSuppressionResponse,
    DeleteAuditSuppressionResponse,

    -- * Response Lenses
    dasrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAuditSuppression' smart constructor.
data DeleteAuditSuppression = DeleteAuditSuppression'
  { _dasCheckName ::
      !Text,
    _dasResourceIdentifier :: !ResourceIdentifier
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAuditSuppression' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasCheckName' - Undocumented member.
--
-- * 'dasResourceIdentifier' - Undocumented member.
deleteAuditSuppression ::
  -- | 'dasCheckName'
  Text ->
  -- | 'dasResourceIdentifier'
  ResourceIdentifier ->
  DeleteAuditSuppression
deleteAuditSuppression pCheckName_ pResourceIdentifier_ =
  DeleteAuditSuppression'
    { _dasCheckName = pCheckName_,
      _dasResourceIdentifier = pResourceIdentifier_
    }

-- | Undocumented member.
dasCheckName :: Lens' DeleteAuditSuppression Text
dasCheckName = lens _dasCheckName (\s a -> s {_dasCheckName = a})

-- | Undocumented member.
dasResourceIdentifier :: Lens' DeleteAuditSuppression ResourceIdentifier
dasResourceIdentifier = lens _dasResourceIdentifier (\s a -> s {_dasResourceIdentifier = a})

instance AWSRequest DeleteAuditSuppression where
  type Rs DeleteAuditSuppression = DeleteAuditSuppressionResponse
  request = postJSON ioT
  response =
    receiveEmpty
      ( \s h x ->
          DeleteAuditSuppressionResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteAuditSuppression

instance NFData DeleteAuditSuppression

instance ToHeaders DeleteAuditSuppression where
  toHeaders = const mempty

instance ToJSON DeleteAuditSuppression where
  toJSON DeleteAuditSuppression' {..} =
    object
      ( catMaybes
          [ Just ("checkName" .= _dasCheckName),
            Just ("resourceIdentifier" .= _dasResourceIdentifier)
          ]
      )

instance ToPath DeleteAuditSuppression where
  toPath = const "/audit/suppressions/delete"

instance ToQuery DeleteAuditSuppression where
  toQuery = const mempty

-- | /See:/ 'deleteAuditSuppressionResponse' smart constructor.
newtype DeleteAuditSuppressionResponse = DeleteAuditSuppressionResponse'
  { _dasrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAuditSuppressionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasrsResponseStatus' - -- | The response status code.
deleteAuditSuppressionResponse ::
  -- | 'dasrsResponseStatus'
  Int ->
  DeleteAuditSuppressionResponse
deleteAuditSuppressionResponse pResponseStatus_ =
  DeleteAuditSuppressionResponse'
    { _dasrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dasrsResponseStatus :: Lens' DeleteAuditSuppressionResponse Int
dasrsResponseStatus = lens _dasrsResponseStatus (\s a -> s {_dasrsResponseStatus = a})

instance NFData DeleteAuditSuppressionResponse
