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
-- Module      : Network.AWS.CostAndUsageReport.ModifyReportDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to programatically update your report preferences.
module Network.AWS.CostAndUsageReport.ModifyReportDefinition
  ( -- * Creating a Request
    modifyReportDefinition,
    ModifyReportDefinition,

    -- * Request Lenses
    mrdReportName,
    mrdReportDefinition,

    -- * Destructuring the Response
    modifyReportDefinitionResponse,
    ModifyReportDefinitionResponse,

    -- * Response Lenses
    mrdrsResponseStatus,
  )
where

import Network.AWS.CostAndUsageReport.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyReportDefinition' smart constructor.
data ModifyReportDefinition = ModifyReportDefinition'
  { _mrdReportName ::
      !Text,
    _mrdReportDefinition :: !ReportDefinition
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyReportDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrdReportName' - Undocumented member.
--
-- * 'mrdReportDefinition' - Undocumented member.
modifyReportDefinition ::
  -- | 'mrdReportName'
  Text ->
  -- | 'mrdReportDefinition'
  ReportDefinition ->
  ModifyReportDefinition
modifyReportDefinition pReportName_ pReportDefinition_ =
  ModifyReportDefinition'
    { _mrdReportName = pReportName_,
      _mrdReportDefinition = pReportDefinition_
    }

-- | Undocumented member.
mrdReportName :: Lens' ModifyReportDefinition Text
mrdReportName = lens _mrdReportName (\s a -> s {_mrdReportName = a})

-- | Undocumented member.
mrdReportDefinition :: Lens' ModifyReportDefinition ReportDefinition
mrdReportDefinition = lens _mrdReportDefinition (\s a -> s {_mrdReportDefinition = a})

instance AWSRequest ModifyReportDefinition where
  type Rs ModifyReportDefinition = ModifyReportDefinitionResponse
  request = postJSON costAndUsageReport
  response =
    receiveEmpty
      ( \s h x ->
          ModifyReportDefinitionResponse' <$> (pure (fromEnum s))
      )

instance Hashable ModifyReportDefinition

instance NFData ModifyReportDefinition

instance ToHeaders ModifyReportDefinition where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSOrigamiServiceGatewayService.ModifyReportDefinition" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ModifyReportDefinition where
  toJSON ModifyReportDefinition' {..} =
    object
      ( catMaybes
          [ Just ("ReportName" .= _mrdReportName),
            Just ("ReportDefinition" .= _mrdReportDefinition)
          ]
      )

instance ToPath ModifyReportDefinition where
  toPath = const "/"

instance ToQuery ModifyReportDefinition where
  toQuery = const mempty

-- | /See:/ 'modifyReportDefinitionResponse' smart constructor.
newtype ModifyReportDefinitionResponse = ModifyReportDefinitionResponse'
  { _mrdrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyReportDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrdrsResponseStatus' - -- | The response status code.
modifyReportDefinitionResponse ::
  -- | 'mrdrsResponseStatus'
  Int ->
  ModifyReportDefinitionResponse
modifyReportDefinitionResponse pResponseStatus_ =
  ModifyReportDefinitionResponse'
    { _mrdrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
mrdrsResponseStatus :: Lens' ModifyReportDefinitionResponse Int
mrdrsResponseStatus = lens _mrdrsResponseStatus (\s a -> s {_mrdrsResponseStatus = a})

instance NFData ModifyReportDefinitionResponse
