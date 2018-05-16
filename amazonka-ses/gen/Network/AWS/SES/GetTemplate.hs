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
-- Module      : Network.AWS.SES.GetTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the template object (which includes the Subject line, HTML part and text part) for the template you specify.
--
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.GetTemplate
    (
    -- * Creating a Request
      getTemplate
    , GetTemplate
    -- * Request Lenses
    , gtTemplateName

    -- * Destructuring the Response
    , getTemplateResponse
    , GetTemplateResponse
    -- * Response Lenses
    , gtrsTemplate
    , gtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | /See:/ 'getTemplate' smart constructor.
newtype GetTemplate = GetTemplate'
  { _gtTemplateName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtTemplateName' - The name of the template you want to retrieve.
getTemplate
    :: Text -- ^ 'gtTemplateName'
    -> GetTemplate
getTemplate pTemplateName_ = GetTemplate' {_gtTemplateName = pTemplateName_}


-- | The name of the template you want to retrieve.
gtTemplateName :: Lens' GetTemplate Text
gtTemplateName = lens _gtTemplateName (\ s a -> s{_gtTemplateName = a})

instance AWSRequest GetTemplate where
        type Rs GetTemplate = GetTemplateResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "GetTemplateResult"
              (\ s h x ->
                 GetTemplateResponse' <$>
                   (x .@? "Template") <*> (pure (fromEnum s)))

instance Hashable GetTemplate where

instance NFData GetTemplate where

instance ToHeaders GetTemplate where
        toHeaders = const mempty

instance ToPath GetTemplate where
        toPath = const "/"

instance ToQuery GetTemplate where
        toQuery GetTemplate'{..}
          = mconcat
              ["Action" =: ("GetTemplate" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "TemplateName" =: _gtTemplateName]

-- | /See:/ 'getTemplateResponse' smart constructor.
data GetTemplateResponse = GetTemplateResponse'
  { _gtrsTemplate       :: !(Maybe Template)
  , _gtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrsTemplate' - Undocumented member.
--
-- * 'gtrsResponseStatus' - -- | The response status code.
getTemplateResponse
    :: Int -- ^ 'gtrsResponseStatus'
    -> GetTemplateResponse
getTemplateResponse pResponseStatus_ =
  GetTemplateResponse'
    {_gtrsTemplate = Nothing, _gtrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
gtrsTemplate :: Lens' GetTemplateResponse (Maybe Template)
gtrsTemplate = lens _gtrsTemplate (\ s a -> s{_gtrsTemplate = a})

-- | -- | The response status code.
gtrsResponseStatus :: Lens' GetTemplateResponse Int
gtrsResponseStatus = lens _gtrsResponseStatus (\ s a -> s{_gtrsResponseStatus = a})

instance NFData GetTemplateResponse where
