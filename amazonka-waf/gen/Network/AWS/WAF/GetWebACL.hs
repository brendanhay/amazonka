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
-- Module      : Network.AWS.WAF.GetWebACL
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'WebACL' that is specified by @WebACLId@ .
--
--
module Network.AWS.WAF.GetWebACL
    (
    -- * Creating a Request
      getWebACL
    , GetWebACL
    -- * Request Lenses
    , gwaWebACLId

    -- * Destructuring the Response
    , getWebACLResponse
    , GetWebACLResponse
    -- * Response Lenses
    , gwarsWebACL
    , gwarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'getWebACL' smart constructor.
newtype GetWebACL = GetWebACL'
  { _gwaWebACLId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetWebACL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gwaWebACLId' - The @WebACLId@ of the 'WebACL' that you want to get. @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
getWebACL
    :: Text -- ^ 'gwaWebACLId'
    -> GetWebACL
getWebACL pWebACLId_ = GetWebACL' {_gwaWebACLId = pWebACLId_}


-- | The @WebACLId@ of the 'WebACL' that you want to get. @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
gwaWebACLId :: Lens' GetWebACL Text
gwaWebACLId = lens _gwaWebACLId (\ s a -> s{_gwaWebACLId = a})

instance AWSRequest GetWebACL where
        type Rs GetWebACL = GetWebACLResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 GetWebACLResponse' <$>
                   (x .?> "WebACL") <*> (pure (fromEnum s)))

instance Hashable GetWebACL where

instance NFData GetWebACL where

instance ToHeaders GetWebACL where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.GetWebACL" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetWebACL where
        toJSON GetWebACL'{..}
          = object
              (catMaybes [Just ("WebACLId" .= _gwaWebACLId)])

instance ToPath GetWebACL where
        toPath = const "/"

instance ToQuery GetWebACL where
        toQuery = const mempty

-- | /See:/ 'getWebACLResponse' smart constructor.
data GetWebACLResponse = GetWebACLResponse'
  { _gwarsWebACL         :: !(Maybe WebACL)
  , _gwarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetWebACLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gwarsWebACL' - Information about the 'WebACL' that you specified in the @GetWebACL@ request. For more information, see the following topics:     * 'WebACL' : Contains @DefaultAction@ , @MetricName@ , @Name@ , an array of @Rule@ objects, and @WebACLId@      * @DefaultAction@ (Data type is 'WafAction' ): Contains @Type@      * @Rules@ : Contains an array of @ActivatedRule@ objects, which contain @Action@ , @Priority@ , and @RuleId@      * @Action@ : Contains @Type@
--
-- * 'gwarsResponseStatus' - -- | The response status code.
getWebACLResponse
    :: Int -- ^ 'gwarsResponseStatus'
    -> GetWebACLResponse
getWebACLResponse pResponseStatus_ =
  GetWebACLResponse'
    {_gwarsWebACL = Nothing, _gwarsResponseStatus = pResponseStatus_}


-- | Information about the 'WebACL' that you specified in the @GetWebACL@ request. For more information, see the following topics:     * 'WebACL' : Contains @DefaultAction@ , @MetricName@ , @Name@ , an array of @Rule@ objects, and @WebACLId@      * @DefaultAction@ (Data type is 'WafAction' ): Contains @Type@      * @Rules@ : Contains an array of @ActivatedRule@ objects, which contain @Action@ , @Priority@ , and @RuleId@      * @Action@ : Contains @Type@
gwarsWebACL :: Lens' GetWebACLResponse (Maybe WebACL)
gwarsWebACL = lens _gwarsWebACL (\ s a -> s{_gwarsWebACL = a})

-- | -- | The response status code.
gwarsResponseStatus :: Lens' GetWebACLResponse Int
gwarsResponseStatus = lens _gwarsResponseStatus (\ s a -> s{_gwarsResponseStatus = a})

instance NFData GetWebACLResponse where
