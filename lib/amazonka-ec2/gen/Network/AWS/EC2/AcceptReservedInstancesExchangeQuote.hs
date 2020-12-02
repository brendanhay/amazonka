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
-- Module      : Network.AWS.EC2.AcceptReservedInstancesExchangeQuote
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts the Convertible Reserved Instance exchange quote described in the 'GetReservedInstancesExchangeQuote' call.
--
--
module Network.AWS.EC2.AcceptReservedInstancesExchangeQuote
    (
    -- * Creating a Request
      acceptReservedInstancesExchangeQuote
    , AcceptReservedInstancesExchangeQuote
    -- * Request Lenses
    , arieqTargetConfigurations
    , arieqDryRun
    , arieqReservedInstanceIds

    -- * Destructuring the Response
    , acceptReservedInstancesExchangeQuoteResponse
    , AcceptReservedInstancesExchangeQuoteResponse
    -- * Response Lenses
    , arieqrsExchangeId
    , arieqrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for accepting the quote.
--
--
--
-- /See:/ 'acceptReservedInstancesExchangeQuote' smart constructor.
data AcceptReservedInstancesExchangeQuote = AcceptReservedInstancesExchangeQuote'
  { _arieqTargetConfigurations :: !(Maybe [TargetConfigurationRequest])
  , _arieqDryRun               :: !(Maybe Bool)
  , _arieqReservedInstanceIds  :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptReservedInstancesExchangeQuote' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arieqTargetConfigurations' - The configuration of the target Convertible Reserved Instance to exchange for your current Convertible Reserved Instances.
--
-- * 'arieqDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'arieqReservedInstanceIds' - The IDs of the Convertible Reserved Instances to exchange for another Convertible Reserved Instance of the same or higher value.
acceptReservedInstancesExchangeQuote
    :: AcceptReservedInstancesExchangeQuote
acceptReservedInstancesExchangeQuote =
  AcceptReservedInstancesExchangeQuote'
    { _arieqTargetConfigurations = Nothing
    , _arieqDryRun = Nothing
    , _arieqReservedInstanceIds = mempty
    }


-- | The configuration of the target Convertible Reserved Instance to exchange for your current Convertible Reserved Instances.
arieqTargetConfigurations :: Lens' AcceptReservedInstancesExchangeQuote [TargetConfigurationRequest]
arieqTargetConfigurations = lens _arieqTargetConfigurations (\ s a -> s{_arieqTargetConfigurations = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
arieqDryRun :: Lens' AcceptReservedInstancesExchangeQuote (Maybe Bool)
arieqDryRun = lens _arieqDryRun (\ s a -> s{_arieqDryRun = a})

-- | The IDs of the Convertible Reserved Instances to exchange for another Convertible Reserved Instance of the same or higher value.
arieqReservedInstanceIds :: Lens' AcceptReservedInstancesExchangeQuote [Text]
arieqReservedInstanceIds = lens _arieqReservedInstanceIds (\ s a -> s{_arieqReservedInstanceIds = a}) . _Coerce

instance AWSRequest
           AcceptReservedInstancesExchangeQuote
         where
        type Rs AcceptReservedInstancesExchangeQuote =
             AcceptReservedInstancesExchangeQuoteResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 AcceptReservedInstancesExchangeQuoteResponse' <$>
                   (x .@? "exchangeId") <*> (pure (fromEnum s)))

instance Hashable
           AcceptReservedInstancesExchangeQuote
         where

instance NFData AcceptReservedInstancesExchangeQuote
         where

instance ToHeaders
           AcceptReservedInstancesExchangeQuote
         where
        toHeaders = const mempty

instance ToPath AcceptReservedInstancesExchangeQuote
         where
        toPath = const "/"

instance ToQuery AcceptReservedInstancesExchangeQuote
         where
        toQuery AcceptReservedInstancesExchangeQuote'{..}
          = mconcat
              ["Action" =:
                 ("AcceptReservedInstancesExchangeQuote" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "TargetConfiguration" <$>
                    _arieqTargetConfigurations),
               "DryRun" =: _arieqDryRun,
               toQueryList "ReservedInstanceId"
                 _arieqReservedInstanceIds]

-- | The result of the exchange and whether it was @successful@ .
--
--
--
-- /See:/ 'acceptReservedInstancesExchangeQuoteResponse' smart constructor.
data AcceptReservedInstancesExchangeQuoteResponse = AcceptReservedInstancesExchangeQuoteResponse'
  { _arieqrsExchangeId     :: !(Maybe Text)
  , _arieqrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptReservedInstancesExchangeQuoteResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arieqrsExchangeId' - The ID of the successful exchange.
--
-- * 'arieqrsResponseStatus' - -- | The response status code.
acceptReservedInstancesExchangeQuoteResponse
    :: Int -- ^ 'arieqrsResponseStatus'
    -> AcceptReservedInstancesExchangeQuoteResponse
acceptReservedInstancesExchangeQuoteResponse pResponseStatus_ =
  AcceptReservedInstancesExchangeQuoteResponse'
    {_arieqrsExchangeId = Nothing, _arieqrsResponseStatus = pResponseStatus_}


-- | The ID of the successful exchange.
arieqrsExchangeId :: Lens' AcceptReservedInstancesExchangeQuoteResponse (Maybe Text)
arieqrsExchangeId = lens _arieqrsExchangeId (\ s a -> s{_arieqrsExchangeId = a})

-- | -- | The response status code.
arieqrsResponseStatus :: Lens' AcceptReservedInstancesExchangeQuoteResponse Int
arieqrsResponseStatus = lens _arieqrsResponseStatus (\ s a -> s{_arieqrsResponseStatus = a})

instance NFData
           AcceptReservedInstancesExchangeQuoteResponse
         where
